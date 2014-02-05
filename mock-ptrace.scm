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

;; This file provides the basics for writing gdb experiments and tests.

;; N.B. This file is always in a state of disrepair.  Hopefully less over time.

(define-module (mock-ptrace)
  #:use-module ((wrap-syscall) #:renamer (symbol-prefix-proc 'wrap-syscall:))
  #:use-module ((gdb) #:select (error-port))
)

(define (initialize!)
  (wrap-syscall:initialize!))

(define (enable!)
  (wrap-syscall:enable! 'ptrace wrap-ptrace
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

(define (disable!)
  (wrap-syscall:enable! 'ptrace #f
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

;; Log output is sent to gdb's error port as that's what gdb_stdlog really is,
;; and we don't have log-port yet.

(define (wrap-ptrace request pid addr data)
  (let ((result (wrap-syscall:real-ptrace request pid addr data)))
    (format (error-port) "ptrace (~a (~a), ~a, ~a, ~a) -> ~a\n"
	    request (wrap-syscall:lookup-ptrace-name request) pid addr data result)
    (set-thread-state! (get-thread! pid) (ptrace-command->thread-state request))
    result))

(define (wrap-waitpid pid stat_ptr options)
  (let* ((result (wrap-syscall:real-waitpid pid stat_ptr options))
	 (status (wrap-syscall:get-wait-status stat_ptr)))
    (format (error-port) "waitpid (~a, ~a, ~a) -> ~a, ~a\n"
	    pid stat_ptr options result status)
    (if (not (= (car result) -1))
	(let ((pid (car result))
	      (thread (get-thread! pid)))
	  (set-thread-state! thread 'stopped)
	  (if (= status (wrap-syscall:make-stopped *signal-sigstop*))
	      (reset-thread-sigstopped! thread))))
    result))

(define (wrap-kill pid sig)
  (let ((result (wrap-syscall:real-kill pid sig)))
    (format (error-port) "kill (~a, ~a) -> ~a\n" pid sig result)
    (if (= *signal-sigstop* sig)
	(set-thread-sigstopped! (get-thread! pid)))
    result))

(define (wrap-fork)
  (let ((result (wrap-syscall:real-fork)))
    (format (error-port) "fork () -> ~a\n" result)
    result))

(define (wrap-vfork)
  (let ((result (wrap-syscall:real-vfork)))
    (format (error-port) "vfork () -> ~a\n" result)
    result))

(define (wrap-open pathname flags mode)
  (let ((result (wrap-syscall:real-open pathname flags mode)))
    (format (error-port) "open (~a, ~a, ~a) -> ~a\n" pathname flags mode result)
    result))

(define (wrap-open64 pathname flags mode)
  (let ((result (wrap-syscall:real-open64 pathname flags mode)))
    (format (error-port) "open64 (~a, ~a, ~a) -> ~a\n" pathname flags mode result)
    result))

(define (wrap-read fd buf count)
  (let ((result (wrap-syscall:real-read fd buf count)))
    (if (not (= fd 0))
	(format (error-port) "read (~a, ~a, ~a) -> ~a\n" fd buf count result))
    result))

(define (wrap-pread64 fd buf count offset)
  (let ((result (wrap-syscall:real-pread64 fd buf count offset)))
    (format (error-port) "pread64 (~a, ~a, ~a, ~a) -> ~a\n" fd buf count offset result)
    result))

(define (wrap-close fd)
  (let ((result (wrap-syscall:real-close fd)))
    (format (error-port) "close (~a) -> ~a\n" fd result)
    result))

(define (wrap-syscall syscall-nr arg1 arg2 arg3)
  (let ((result (wrap-syscall:real-syscall syscall-nr arg1 arg2 arg3)))
    (display (wrap-syscall:format-syscall-call syscall-nr arg1 arg2 arg3 result) (error-port))
    (if (and (= *syscall-tkill* syscall-nr)
	     (= *signal-sigstop* arg2))
	(set-thread-sigstopped! (get-thread! arg1)))
    result))

(define *thread-table* #f)

(define (reset-thread-table!)
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

(define *syscall-tkill* (wrap-syscall:lookup-syscall-nr 'tkill))

(define *signal-sigstop* (wrap-syscall:lookup-signal-nr 'SIGSTOP))

(define *signal-sigusr1* (wrap-syscall:lookup-signal-nr 'SIGUSR1))

(define (reset-all-thread-sigstopped!)
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
  (let ((request-name (wrap-syscall:lookup-ptrace-name request)))
    (case request-name
      ((PTRACE_SINGLESTEP) 'stepping)
      ((PTRACE_CONT) 'running)
      (else #f))))

(export
 initialize!
 enable!
 disable!
 reset-thread-table!
 reset-all-thread-sigstopped!
 set-thread-sigstopped!
 get-thread!
 set-thread-state!
 ptrace-command->thread-state

 *syscall-tkill*
 *signal-sigstop*
 *signal-sigusr1*
)
