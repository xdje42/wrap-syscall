;; Experimental user of wrap-syscall.
;; Copyright (C) 2014 Free Software Foundation, Inc.
;;
;; This file is part of wrap-syscall.
;;
;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation; either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; N.B. This file is always in a state of disrepair.

(define-module (mock-ptrace)
  #:use-module (wrap-syscall)
)

(define (mock-ptrace-initialize!)
  (wrap-syscall-initialize!))

(define (mock-ptrace-enable!)
  (wrap-syscall-enable! 'ptrace wrap-ptrace
			'waitpid wrap-waitpid
			'kill wrap-kill
			'fork wrap-fork
			'vfork wrap-vfork
			'open wrap-open
			'open64 wrap-open64
			'read wrap-read
			'pread64 wrap-pread64
			'close wrap-close
			'syscall wrap-syscall
			))

(define (mock-ptrace-disable!)
  (wrap-syscall-enable! 'ptrace #f
			'waitpid #f
			'kill #f
			'fork #f
			'vfork #f
			'open #f
			'open64 #f
			'read #f
			'pread64 #f
			'close #f
			'syscall #f
			))

(define *inject-sigusr1* #f)

(define (wrap-ptrace request pid addr data)
  (let ((result (real-ptrace request pid addr data)))
    (format #t "ptrace (~a (~a), ~a, ~a, ~a) -> ~a\n"
	    request (lookup-ptrace-name request) pid addr data result)
    (set-thread-state! (get-thread! pid) (ptrace-command->thread-state request))
    result))

(define (wrap-waitpid pid stat_ptr options)
  (if (and #f
	   *inject-sigusr1*
	   (> pid 0)
	   (thread-sigstopped? (get-thread! pid))
	   (logand options (lookup-wait-flag '__WCLONE)))

      (let ((result (cons pid 0))
	    (status (make-stopped (lookup-signal-nr 'SIGUSR1))))
	(set-wait-status! stat_ptr status)
	(set! *inject-sigusr1* #f) ;; one shot for now
	(format #t "waitpid (~a, ~a, ~a) -> ~a, ~a (SIGUSR1 injected)\n"
		pid stat_ptr options result status)
	result)

      (let* ((result (real-waitpid pid stat_ptr options))
	     (status (get-wait-status stat_ptr)))
	(format #t "waitpid (~a, ~a, ~a) -> ~a, ~a\n"
		pid stat_ptr options result status)
	(if (not (= (car result) -1))
	    (let ((pid (car result))
		  (thread (get-thread! pid)))
	      (set-thread-state! thread 'stopped)
	      (if (= status (make-stopped *signal-sigstop*))
		  (reset-thread-sigstopped! thread))))
	result)))

(define (wrap-kill pid sig)
  (let ((result (real-kill pid sig)))
    (format #t "kill (~a, ~a) -> ~a\n" pid sig result)
    (if (= *signal-sigstop* sig)
	(set-thread-sigstopped! (get-thread! arg1)))
    result))

(define (wrap-fork)
  (let ((result (real-fork)))
    (format #t "fork () -> ~a\n" result)
    result))

(define (wrap-vfork)
  (let ((result (real-vfork)))
    (format #t "vfork () -> ~a\n" result)
    result))

(define (wrap-open pathname flags mode)
  (let ((result (real-open pathname flags mode)))
    (format #t "open (~a, ~a, ~a) -> ~a\n" pathname flags mode result)
    result))

(define (wrap-open64 pathname flags mode)
  (let ((result (real-open64 pathname flags mode)))
    (format #t "open64 (~a, ~a, ~a) -> ~a\n" pathname flags mode result)
    result))

(define (wrap-read fd buf count)
  (let ((result (real-read fd buf count)))
    (if (not (= fd 0))
	(format #t "read (~a, ~a, ~a) -> ~a\n" fd buf count result))
    result))

(define (wrap-pread64 fd buf count offset)
  (let ((result (real-pread64 fd buf count offset)))
    (format #t "pread64 (~a, ~a, ~a, ~a) -> ~a\n" fd buf count offset result)
    result))

(define (wrap-close fd)
  (let ((result (real-close fd)))
    (format #t "close (~a) -> ~a\n" fd result)
    result))

(define (wrap-syscall syscall-nr arg1 arg2 arg3)
  (if (and #t
	   *inject-sigusr1*
	   (or (eq? *inject-sigusr1* #t) (eq? *inject-sigusr1* arg1))
	   (= syscall-nr *syscall-tkill*)
	   (= arg2 *signal-sigstop*))
      (begin
	;;(set! *inject-sigusr1* #f) ;; one shot for now
	(set! *inject-sigusr1* arg1) ;; keep sending to this thread only
	(format #t "Injecting SIGUSR1 to ~a ...\n" arg1)
	(real-syscall syscall-nr arg1 *signal-sigusr1* arg3)))
  (let ((result (real-syscall syscall-nr arg1 arg2 arg3)))
    (display (format-syscall-call syscall-nr arg1 arg2 arg3 result))
    (if (and (= *syscall-tkill* syscall-nr)
	     (= *signal-sigstop* arg2))
	(set-thread-sigstopped! (get-thread! arg1)))
    result))

(define *thread-table* #f)

(define-public (mock-ptrace-reset-thread-table!)
  (set! *thread-table* (make-hash-table))
)

;; Threads are recorded as (#t state SIGSTOP-sent?)
;; state is one of: init, running, stepping, stopped

(define (make-thread) (list #t 'init #f))

(define (thread-state t) (cadr t))

(define (set-thread-state! t new-state)
  (list-set! t 1 new-state))

(define (reset-thread-sigstopped! t) (list-set! t 2 #f))

(define (set-thread-sigstopped! t) (list-set! t 2 #t))

(define (thread-sigstopped? t) (list-ref t 2))

(define *syscall-tkill* (lookup-syscall-nr 'tkill))

(define *signal-sigstop* (lookup-signal-nr 'SIGSTOP))

(define *signal-sigusr1* (lookup-signal-nr 'SIGUSR1))

(define (mock-ptrace-reset-all-thread-sigstopped!)
  (hash-for-each (lambda (key value)
		   (reset-thread-sigstopped! value))
		 *thread-table*))

(define (get-thread! lwpid)
  (or (hashq-ref *thread-table* lwpid)
      (add-thread! lwpid)))

(define (add-thread! lwpid)
  (let ((thread (make-thread)))
    (hashq-set! *thread-table* lwpid thread)
    thread)
)

(define (ptrace-command->thread-state request)
  (let ((request-name (lookup-ptrace-name request)))
    (case request-name
      ((PTRACE_SINGLESTEP) 'stepping)
      ((PTRACE_CONT) 'running)
      (else #f)))
)

(export
 mock-ptrace-initialize!
 mock-ptrace-enable!
 mock-ptrace-disable!
 mock-ptrace-reset-thread-table!
 mock-ptrace-reset-all-thread-sigstopped!
 *inject-sigusr1*
)
