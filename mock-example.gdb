# Experimental user of wrap-syscall.
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
  gu (use-modules ((wrap-syscall) #:renamer (symbol-prefix-proc 'wrap-syscall:)))
  gu (use-modules ((mock-ptrace) #:renamer (symbol-prefix-proc 'mock-ptrace:)))
end

# Our own syscall/tkill wrapper to inject SIGUSR1.
guile
(define *inject-sigusr1* #f)

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
end

file mock-test-app.x

eval "attach %d", $pid

b break_here
c

if ! $strace
  gu (mock-ptrace:initialize!)
  gu (mock-ptrace:reset-thread-table!)
  gu (mock-ptrace:enable!)
  # Install our own syscall wrapper.
  gu (wrap-syscall:enable! 'syscall my-wrap-syscall)
end

if 1
  c

  if ! $strace
    gu (set! *inject-sigusr1* #t)
    set debug infrun 1
    set debug lin-lwp 1
  end

  set $n = 10
  set $i = 0
  while $i < $n
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
