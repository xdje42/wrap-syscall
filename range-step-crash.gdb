# Testcase for crash when SIGPROF arrives while range-stepping,
# having stepped into a subroutine call.
# Copyright (C) 2014 Free Software Foundation, Inc.
#
# This file is part of wrap-syscall.
#
# This program is free software; you can redistribute it and/or modify
# it under the terms of the GNU General Public License as published by
# the Free Software Foundation; either version 3 of the License, or
# (at your option) any later version.
#
# This program is distributed in the hope that it will be useful,
# but WITHOUT ANY WARRANTY; without even the implied warranty of
# MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
# GNU General Public License for more details.
#
# You should have received a copy of the GNU General Public License
# along with this program.  If not, see <http://www.gnu.org/licenses/>.

# We require the test harness to have already set:
# convenience variables: $pid, $strace
# guile variable: srcdir

# N.B. This file is always in a state of disrepair.

set trace-commands on

unset env LD_PRELOAD

handle SIGUSR1 nostop print pass

if ! $strace
  set guile print-stack full
  gu (add-to-load-path srcdir)
  gu (add-to-load-path (getcwd))
  gu (use-modules (gdb))
  gu (use-modules (ice-9 regex))
  gu (use-modules ((wrap-syscall) #:renamer (symbol-prefix-proc 'wrap-syscall:)))
  gu (use-modules ((mock-ptrace) #:renamer (symbol-prefix-proc 'mock-ptrace:)))
end

guile
(define *ptrace-cont* (wrap-syscall:lookup-ptrace-nr 'PTRACE_CONT))
(define *ptrace-singlestep* (wrap-syscall:lookup-ptrace-nr 'PTRACE_SINGLESTEP))

(define *inject-sigusr1* #f)
(define *inject-at-step-resume* #f)
(define *inject-step-lwp* #f)
(define *inject-step-count* #f)

;; We need to watch ptrace so we can count singlesteps.

(define (my-wrap-ptrace request pid addr data)
  (if (and #f
	   *inject-sigusr1*
	   *inject-at-step-resume*
	   (= *inject-at-step-resume* pid)
	   (= request *ptrace-cont*))
      (begin
	(format (error-port) "Injecting SIGUSR1 to ~a ...\n" *inject-sigusr1*)
	(wrap-syscall:real-syscall mock-ptrace:*syscall-tkill* *inject-sigusr1* mock-ptrace:*signal-sigusr1* 0)
	(set! *inject-sigusr1* #f) ;; one-shot
	))
  (if (and #t
	   *inject-sigusr1*
	   *inject-step-lwp*
	   (= *inject-step-lwp* pid)
	   (> *inject-step-count* 0)
	   (= request *ptrace-singlestep*))
      (begin
	(set! *inject-step-count* (- *inject-step-count* 1))
	(if (= *inject-step-count* 0)
	    (begin
	      (format (error-port) "Injecting SIGUSR1 to ~a ...\n" *inject-sigusr1*)
	      (wrap-syscall:real-syscall mock-ptrace:*syscall-tkill* *inject-sigusr1* mock-ptrace:*signal-sigusr1* 0)))
	))
  (let ((result (wrap-syscall:real-ptrace request pid addr data)))
    (format (error-port) "ptrace (~a (~a), ~a, ~a, ~a) -> ~a\n"
	    request (wrap-syscall:lookup-ptrace-name request) pid addr data result)
    (mock-ptrace:set-thread-state! (mock-ptrace:get-thread! pid) (mock-ptrace:ptrace-command->thread-state request))
    result))

(define (my-wrap-syscall syscall-nr arg1 arg2 arg3)
  (if (and #t
	   *inject-sigusr1*
	   (or (eq? *inject-sigusr1* #t) (eq? *inject-sigusr1* arg1))
	   (= syscall-nr mock-ptrace:*syscall-tkill*)
	   (= arg2 mock-ptrace:*signal-sigstop*))
      (begin
	;;(set! *inject-sigusr1* #f) ;; one shot for now
	(set! *inject-sigusr1* arg1) ;; keep sending to this thread only
	(format (error-port) "Injecting SIGUSR1 to ~a ...\n" arg1)
	(wrap-syscall:real-syscall syscall-nr arg1 mock-ptrace:*signal-sigusr1* arg3)))
  (let ((result (wrap-syscall:real-syscall syscall-nr arg1 arg2 arg3)))
    (display (wrap-syscall:format-syscall-call syscall-nr arg1 arg2 arg3 result) (error-port))
    (if (and (= mock-ptrace:*syscall-tkill* syscall-nr)
	     (= mock-ptrace:*signal-sigstop* arg2))
	(mock-ptrace:set-thread-sigstopped! (mock-ptrace:get-thread! arg1)))
    result))

(define (get-thread-lwp thread-nr)
  (let ((output (execute (format #f "info thread ~a" thread-nr) #:to-string #t)))
    (string->number (match:substring (string-match "\\(LWP ([0-9]+)\\)" output) 1))))
end

file range-step-crash.x

eval "attach %d", $pid

break break_here_before_inc thread 2
thread 2
c

i thr

if ! $strace
  gu (mock-ptrace:initialize!)
  gu (mock-ptrace:reset-thread-table!)
  gu (mock-ptrace:enable!)
  # Install our own ptrace,syscall wrappers.
  gu (wrap-syscall:enable! 'ptrace my-wrap-ptrace 'syscall my-wrap-syscall)
end

gu (define pid (value->integer (parse-and-eval "$pid")))
gu (define thr2 (get-thread-lwp 2))
gu (define thr3 (get-thread-lwp 3))

if 1
  c

  if ! $strace
    set debug infrun 1
    set debug lin-lwp 1
    #gu (set! *inject-sigusr1* #t) ;; #t or pid
  end

  set $n = 10
  set $i = 0
  while $i < $n
    echo Stepping out ...\n
    finish
    next
    echo Next over inc_x ...\n
    gu (set! *inject-sigusr1* thr3)
    #gu (set! *inject-at-step-resume* thr2)
    gu (set! *inject-step-lwp* thr2)
    gu (set! *inject-step-count* 3)
    next
    gu (set! *inject-sigusr1* #f)
    gu (set! *inject-at-step-resume* #f)
    gu (set! *inject-step-lwp* #f)
    gu (set! *inject-step-count* #f)
    echo Continuing ...\n
    continue
    set $i = $i + 1
  end

  if ! $strace
    gu (set! *inject-sigusr1* #f)
    gu (mock-ptrace:disable!)
  end
end

echo Done.\n
