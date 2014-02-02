;; The wrap-syscall Guile module.
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

(define-module (wrap-syscall))

(define *wrap-syscall-dl* #f)

;; This is really a function defined in C.
(define enable! #f)

(define (wrap-syscall-initialize-dl!)
  (setenv "LD_LIBRARY_PATH" (string-append (getenv "HOME") "/gdb/github/wrap-syscall/b-master"))
  (set! *wrap-syscall-dl* (dynamic-link "wrap-syscall"))
  (dynamic-call (dynamic-func "wrap_syscall_initialize" *wrap-syscall-dl*) #f)
)

(define (wrap-syscall-initialize!)
  ;; Perform the initialization in our module so Scheme functions defined
  ;; in C appear in our module.
  (eval '(wrap-syscall-initialize-dl!) (resolve-module '(wrap-syscall)))

  (set! reverse-PTRACE_COMMANDS (reverse-alist PTRACE_COMMANDS))
  (set! reverse-SIGNALS (reverse-alist SIGNALS))
  (set! reverse-SYSCALLS (reverse-alist SYSCALLS))
)

(define-public wrap-syscall-enable! (lambda args (apply enable! args)))

(define (lookup-ptrace-name request)
  (assq-ref reverse-PTRACE_COMMANDS request))

(define (lookup-signal-name signal-nr)
  (assq-ref reverse-SIGNALS signal-nr))

(define (lookup-syscall-name syscall-nr)
  (assq-ref reverse-SYSCALLS syscall-nr))

(define (lookup-wait-flag name)
  (assq-ref WAIT_FLAGS name))

(define (lookup-signal-nr name)
  (assq-ref SIGNALS name))

(define (lookup-syscall-nr name)
  (assq-ref SYSCALLS name))

(define (make-stopped signal-nr)
  (+ (ash signal-nr 8) #x7f))

(define (format-syscall-call syscall-nr arg1 arg2 arg3 result)
  (let ((name (assq-ref reverse-SYSCALLS syscall-nr)))
    (case name
      ((tkill) (if (= arg2 0)
		   (format #f "syscall (~a (~a), ~a, ~a) -> ~a\n"
			   syscall-nr name arg1 arg2 result)
		   (format #f "syscall (~a (~a), ~a, ~a (~a)) -> ~a\n"
			   syscall-nr name arg1 arg2
			   (assq-ref reverse-SIGNALS arg2) result)))
      (else (format #f "syscall (~a, ~a, ~a, ~a) -> ~a\n"
		    syscall-nr arg1 arg2 arg3 result)))))

;; Reverse the elements in a key/value alist (key and value are swapped).

(define (reverse-alist alist)
  (map (lambda (elm) (cons (cdr elm) (car elm)))
       alist))

(define PTRACE_COMMANDS
  '(
    ;; Indicate that the process making this request should be traced.
    ;; All signals received by this process can be intercepted by its
    ;; parent, and its parent can use the other `ptrace' requests.
    (PTRACE_TRACEME . 0)

    ;; Return the word in the process's text space at address ADDR.
    (PTRACE_PEEKTEXT . 1)

    ;; Return the word in the process's data space at address ADDR.
    (PTRACE_PEEKDATA . 2)

    ;; Return the word in the process's user area at offset ADDR.
    (PTRACE_PEEKUSER . 3)

    ;; Write the word DATA into the process's text space at address ADDR.
    (PTRACE_POKETEXT . 4)

    ;; Write the word DATA into the process's data space at address ADDR.
    (PTRACE_POKEDATA . 5)

    ;; Write the word DATA into the process's user area at offset ADDR.
    (PTRACE_POKEUSER . 6)

    ;; Continue the process.
    (PTRACE_CONT . 7)

    ;; Kill the process.
    (PTRACE_KILL . 8)

    ;; Single step the process.
    ;; This is not supported on all machines.
    (PTRACE_SINGLESTEP . 9)

    ;; Get all general purpose registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_GETREGS . 12)

    ;; Set all general purpose registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_SETREGS . 13)

    ;; Get all floating point registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_GETFPREGS . 14)

    ;; Set all floating point registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_SETFPREGS . 15)

    ;; Attach to a process that is already running.
    (PTRACE_ATTACH . 16)

    ;; Detach from a process attached to with PTRACE_ATTACH.
    (PTRACE_DETACH . 17)

    ;; Get all extended floating point registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_GETFPXREGS . 18)

    ;; Set all extended floating point registers used by a processes.
    ;; This is not supported on all machines.
    (PTRACE_SETFPXREGS . 19)

    ;; Continue and stop at the next (return from) syscall.
    (PTRACE_SYSCALL . 24)

    ;; Set ptrace filter options.
    (PTRACE_SETOPTIONS . #x4200)

    ;; Get last ptrace message.
    (PTRACE_GETEVENTMSG . #x4201)

    ;; Get siginfo for process.
    (PTRACE_GETSIGINFO . #x4202)

    ;; Set new siginfo for process.
    (PTRACE_SETSIGINFO . #x4203)

    ;; Get register content.
    (PTRACE_GETREGSET . #x4204)

    ;; Set register content.
    (PTRACE_SETREGSET . #x4205)

    ;; Like PTRACE_ATTACH, but do not force tracee to trap and do not affect
    ;; signal or group stop state.
    (PTRACE_SEIZE . #x4206)

    ;; Trap seized tracee.
    (PTRACE_INTERRUPT . #x4207)

    ;; Wait for next group event.
    (PTRACE_LISTEN . #x4208)
    ))

(define reverse-PTRACE_COMMANDS #f)

;; Flag for PTRACE_LISTEN.

(define PTRACE_FLAGS
  '(
    (PTRACE_SEIZE_DEVEL . #x80000000)
    ))

;; Options set using PTRACE_SETOPTIONS.

(define PTRACE_SETOPTIONS
  '(
    (PTRACE_O_TRACESYSGOOD . #x00000001)
    (PTRACE_O_TRACEFORK . #x00000002)
    (PTRACE_O_TRACEVFORK . #x00000004)
    (PTRACE_O_TRACECLONE . #x00000008)
    (PTRACE_O_TRACEEXEC	. #x00000010)
    (PTRACE_O_TRACEVFORKDONE . #x00000020)
    (PTRACE_O_TRACEEXIT . #x00000040)
    (PTRACE_O_TRACESECCOMP . #x00000080)
    (PTRACE_O_MASK . #x000000ff)
    ))

;; Wait extended result codes for the above trace options.

(define PTRACE_EVENTCODES
  '(
    (PTRACE_EVENT_FORK . 1)
    (PTRACE_EVENT_VFORK . 2)
    (PTRACE_EVENT_CLONE . 3)
    (PTRACE_EVENT_EXEC . 4)
    (PTRACE_EVENT_VFORK_DONE . 5)
    (PTRACE_EVENT_EXIT . 6)
    (PTRAVE_EVENT_SECCOMP . 7)
  ))

(define WAIT_FLAGS
  '(
    (WNOHANG . 1) ;; Don't block waiting.
    (WUNTRACED . 2) ;; Report status of stopped children.

    (WSTOPPED . 2) ;; Report stopped child (same as WUNTRACED). */
    (WEXITED . 4) ;; Report dead child.  */
    (WCONTINUED . 8) ;; Report continued child.  */
    (WNOWAIT . #x01000000) ;; Don't reap, just poll status.  */

    (__WNOTHREAD . #x20000000) ;; Don't wait on children of other threads in this group
    (__WALL . #x40000000) ;; Wait for any child.
    (__WCLONE . #x80000000) ;; Wait for cloned process.
    ))

(define SIGNALS
  '(
    (SIGHUP . 1)
    (SIGINT . 2)
    (SIGQUIT . 3)
    (SIGILL . 4)
    (SIGTRAP . 5)
    (SIGABRT . 6)
    (SIGIOT . 6)
    (SIGBUS . 7)
    (SIGFPE . 8)
    (SIGKILL . 9)
    (SIGUSR1 . 10)
    (SIGSEGV . 11)
    (SIGUSR2 . 12)
    (SIGPIPE . 13)
    (SIGALRM . 14)
    (SIGTERM . 15)
    (SIGSTKFLT . 16)
    (SIGCHLD . 17)
    (SIGCONT . 18)
    (SIGSTOP . 19)
    (SIGTSTP . 20)
    (SIGTTIN . 21)
    (SIGTTOU . 22)
    (SIGURG . 23)
    (SIGXCPU . 24)
    (SIGXFSZ . 25)
    (SIGVTALRM . 26)
    (SIGPROF . 27)
    (SIGWINCH . 28)
    (SIGIO . 29)
    (SIGPOLL . 29)
    (SIGPWR . 30)
    (SIGSYS . 31)
    (SIGUNUSED . 31)
    ))

(define reverse-SIGNALS #f)

(define SYSCALLS
  '(
    (tkill . 200)
    ))

(define reverse-SYSCALLS #f)

(export

 wrap-syscall-initialize!
 wrap-syscall-enable!

 set-wait-status!
 get-wait-status

 lookup-ptrace-name
 lookup-signal-name
 lookup-syscall-name
 lookup-wait-flag
 lookup-signal-nr
 lookup-syscall-nr

 make-stopped

 format-syscall-call

 real-ptrace
 real-waitpid
 real-kill
 real-fork
 real-vfork
 real-open
 real-open64
 real-read
 real-pread64
 real-close
 real-syscall
)
