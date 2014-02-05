;; Generate the various files used by wrap-syscall.
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

;; TODO: have larger strings, keyword substitution in strings
;; TODO: thread safety

(use-modules ((rnrs base) :version (6)))
(use-modules (ice-9 regex))

(define d display)
(define f format)

(define *syscalls*
  '(
    ;; FORMAT: subr name, replacement-or-#f, result type, (arg1-name arg1-type), ...
    ;; "replacement" exists to support hacky workarounds for whatever.

    ("ptrace" #f "long" ("request" "enum __ptrace_request") ("pid" "pid_t") ("addr" "void*") ("data" "void*"))

    ("waitpid" #f "pid_t" ("pid" "pid_t") ("stat_ptr" "int*") ("options" "int"))

    ("kill" #f "int" ("pid" "pid_t") ("sig" "int"))

    ("fork" #f "pid_t")
    ("vfork" "hack_vfork" "pid_t")

    ;; mode is really passed as ... (stdarg), but this is easier.
    ("open" #f "int" ("pathname" "const char*") ("flags" "int") ("mode" "int"))
    ("open64" #f "int" ("pathname" "const char*") ("flags" "int") ("mode" "int"))

    ("read" #f "ssize_t" ("fd" "int") ("buf" "void*") ("count" "size_t"))

    ("pread64" #f "ssize_t" ("fd" "int") ("buf" "void*") ("count" "size_t") ("offset" "off64_t"))

    ("close" #f "int" ("fd" "int"))

    ;; Crap.  We only need this for tkill.
    ;; The actual definition uses stdarg, but this is easier: We need to
    ;; be able to forward the call to another function.
    ("syscall" #f "long" ("syscall_nr" "long") ("arg1" "long") ("arg2" "long") ("arg3" "long"))

    ))

(define syscall-name car)
(define syscall-replacement cadr)
(define syscall-result caddr)
(define syscall-args cdddr)
(define arg-name car)
(define arg-type cadr)

(define *conversions*
  '(
    ("int" "scm_from_int" "scm_to_int" #f)
    ("long" "scm_from_long" "scm_to_long" #f)
    ("pid_t" "scm_from_int" "scm_to_int" #f)
    ("size_t" "scm_from_ulong" "scm_to_ulong" #f)
    ("ssize_t" "scm_from_long" "scm_to_long" #f)
    ("off64_t" "scm_from_int64" "scm_to_uint64" #f)
    ("void*" "ws_scm_from_void_ptr" "ws_scm_to_void_ptr" #f)
    ("int*" "ws_scm_from_void_ptr" "ws_scm_to_void_ptr" #f)
    ("const char*" "scm_from_latin1_string" "scm_to_latin1_string" "free")
    ("enum __ptrace_request" "scm_from_int" "scm_to_int" #f)
    ))

(define (convert-scm-from-type type-name)
  (let ((type (assoc type-name *conversions*)))
    (assert type)
    (cadr type)))

(define (convert-scm-to-type type-name)
  (let ((type (assoc type-name *conversions*)))
    (assert type)
    (caddr type)))

(define (convert-scm-to-type-cleanup type-name)
  (let ((type (assoc type-name *conversions*)))
    (assert type)
    (cadddr type)))

(define (format-arg-defn-list args)
  (if (null? args)
      "void"
      (string-join (map (lambda (arg)
			  (f #f "~a ~a" (arg-type arg) (arg-name arg)))
			args)
		   ", ")))

(define (format-arg-scm-defn-list args)
  (if (null? args)
      "void"
      (string-join (map (lambda (arg)
			  (f #f "SCM ~a" (arg-name arg)))
			args)
		   ", ")))

(define (format-arg-list args)
  (if (null? args)
      ""
      (string-join (map (lambda (arg)
			  (arg-name arg))
			args)
		   ", ")))

(define (format-c-arg-list args)
  (if (null? args)
      ""
      (string-join (map (lambda (arg)
			  (string-append "c_" (arg-name arg)))
			args)
		   ", ")))

(define (format-s-arg-list args)
  (if (null? args)
      ""
      (string-join (map (lambda (arg)
			  (string-append "s_" (arg-name arg)))
			args)
		   ", ")))

(define (format-scm-from-c-locals args)
  (apply string-append
	 (map (lambda (arg)
		(f #f "  SCM s_~a = ~a (~a);\n" (arg-name arg) (convert-scm-from-type (arg-type arg)) (arg-name arg)))
	      args)))

(define (format-scm-to-c-locals args)
  (apply string-append
	 (map (lambda (arg)
		(f #f "  ~a c_~a = ~a (~a);\n" (arg-type arg) (arg-name arg) (convert-scm-to-type (arg-type arg)) (arg-name arg)))
	      args)))

(define (format-scm-to-c-locals-cleanup args)
  (apply string-append
	 (map (lambda (arg)
		(let ((cleanup (convert-scm-to-type-cleanup (arg-type arg))))
		  (if cleanup
		      (f #f "  ~a ((void*) c_~a);\n" cleanup (arg-name arg))
		      "")))
	      args)))

(define (string-keyed-replace string alist)
  (regexp-substitute/global #f "@[a-z-]+@" string
			    'pre
			    (lambda (m)
			      (assq-ref alist (string->symbol (match:substring m))))
			    'post))

(define (echo-args args)
  (d "Howzitgoineh?\n")
  (d args)
  (newline))

(define (gen-ws-gen-h args)
  (assert (= (length args) 1))
  (d "// wrap-syscall generated declarations\n")
  (d "// THIS FILE IS MACHINE-GENERATED.  DO NOT EDIT.\n")

  (newline)
  (d "// typedefs of each function\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "\ntypedef @result@ @name@_ftype (@args@);\n"
		  `((@result@ . ,(syscall-result syscall))
		    (@name@ . ,(syscall-name syscall))
		    (@args@ . ,(format-arg-defn-list (syscall-args syscall)))))))
	    *syscalls*)

  (newline)
  (d "// Wrappers to call the real functions so they can be called from Scheme.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "\nextern @name@_ftype ws_real_@name@;\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)

  (newline)
  (d "// The externally implemented wrappers so they can be called from the\n")
  (d "// preload-inserted wrappers.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "\nextern @name@_ftype* ws_wrap_@name@;\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)
)

(define (gen-ws-preload-gen-c args)
  (assert (= (length args) 1))
  (d "// wrap-syscall preload generated code\n")
  (d "// THIS FILE IS MACHINE-GENERATED.  DO NOT EDIT.\n")

  (newline)
  (d "// asm is used to specify the real function name so we don't have to\n")
  (d "// worry about pedantic conflicts with system headers.\n\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "extern @name@_ftype wrap_@name@ asm (\"@name@\");\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)

  (newline)
  (d "// The addresses of the real functions.\n\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "static @name@_ftype* real_@name@;\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)

  (newline)
  (d "// The Scheme wrappers.\n")
  (d "// These are set later when enable! is called.\n\n");
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "@name@_ftype* ws_wrap_@name@;\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)

  (newline)
  (d "// Lookup all the real functions.\n\n")
  (d "static void\n")
  (d "ws_initialize_real_funcs (void)\n")
  (d "{\n")
  (for-each (lambda (syscall)
	      (let ((name (syscall-name syscall))
		    (replacement (syscall-replacement syscall)))
		(if replacement
		    (d (string-keyed-replace
			"  real_@name@ = @replacement@;\n"
			`((@name@ . ,name)
			  (@replacement@ . ,replacement))))
		    (d (string-keyed-replace
			"  real_@name@ = (@name@_ftype*) dlsym (RTLD_NEXT, \"@name@\");\n"
			`((@name@ . ,name)))))))
	    *syscalls*)
  (d "}\n")

  (newline)
  (d "// The functions that intercept the calls we need.\n")
  (d "// These are installed via LD_PRELOAD.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  ;; { indented one space to not confuse emacs auto-indent
		  "\
\n\
@result@\n\
wrap_@name@ (@arg-defns@)\n\
 {\n\
  if (real_@name@ == NULL)\n\
    ws_initialize_real_funcs ();\n\
\n\
  if (ws_wrap_@name@ != NULL)\n\
    return ws_wrap_@name@ (@arg-list@);\n\
\n\
  return real_@name@ (@arg-list@);\n\
}\n"
		`((@name@ . ,(syscall-name syscall))
		  (@result@ . ,(syscall-result syscall))
		  (@arg-defns@ . ,(format-arg-defn-list (syscall-args syscall)))
		  (@arg-list@ . ,(format-arg-list (syscall-args syscall)))))))
	    *syscalls*)

  (newline)
  (d "// These functions are called from Scheme to perform the real operations.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  ;; { indented one space to not confuse emacs auto-indent
		  "\
\n\
@result@\n\
ws_real_@name@ (@arg-defns@)\n\
 {\n\
  return real_@name@ (@arg-list@);\n\
}\n"
		`((@name@ . ,(syscall-name syscall))
		  (@result@ . ,(syscall-result syscall))
		  (@arg-defns@ . ,(format-arg-defn-list (syscall-args syscall)))
		  (@arg-list@ . ,(format-arg-list (syscall-args syscall)))))))
	    *syscalls*)
)

(define (gen-ws-gen-c args)
  (assert (= (length args) 1))
  (d "// wrap-syscall generated code\n")
  (d "// THIS FILE IS MACHINE-GENERATED.  DO NOT EDIT.\n")

  (newline)
  (d "// These are set to the Scheme wrapper when wrapping is enabled.\n\n")
  (for-each (lambda (syscall)
	      (f #t "static SCM wsscm_wrap_~a = SCM_BOOL_F;\n" (syscall-name syscall)))
	    *syscalls*)

  (newline)
  (d "// C wrapper to call the Scheme function.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  ;; { indented one space to not confuse emacs auto-indent
		  "\
\n\
static @result@\n\
ws_c_wrap_@name@ (@arg-defns@)\n\
 {\n\
@scm-from-c-locals@\
  SCM result;\n\
\n\
  result = wsscm_safe_call_@nr-args@ (wsscm_wrap_@name@@s-arg-list@);\n\
  if (scm_is_false (result))\n\
  {\n\
    errno = EPROTO; // something unlikely to appear otherwise\n\
    return -1;\n\
  }\n\
\n\
  // We assume we get back a pair.  Make robust *later*, if necessary.\n\
  errno = scm_to_int (scm_cdr (result));\n\
  return @scm-to-result@ (scm_car (result));\n\
}\n"
		  `((@name@ . ,(syscall-name syscall))
		    (@result@ . ,(syscall-result syscall))
		    (@arg-defns@ . ,(format-arg-defn-list (syscall-args syscall)))
		    (@scm-from-c-locals@ . ,(format-scm-from-c-locals (syscall-args syscall)))
		    (@nr-args@ . ,(length (syscall-args syscall)))
		    (@s-arg-list@ . ,(if (null? (syscall-args syscall))
					 ""
					 (string-append ", " (format-s-arg-list (syscall-args syscall)))))
		    (@scm-to-result@ . ,(convert-scm-to-type (syscall-result syscall)))))))
	    *syscalls*)

  (newline)
  (d "// Scheme functions to call the real routine.\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  ;; { indented one space to not confuse emacs auto-indent
		  "\
\n\
static SCM\n\
wsscm_real_@name@ (@arg-defns@)\n\
 {\n\
@scm-to-c-locals@\
  @result@ c_result;\n\
  SCM result;\n\
\n\
  errno = 0;\n\
  c_result = ws_real_@name@ (@c-arg-list@);\n\
  result = scm_cons (@scm-from-type@ (c_result), scm_from_int (errno));\n\
@scm-to-c-locals-cleanup@\
  return result;\n\
}\n"
		  `((@name@ . ,(syscall-name syscall))
		    (@result@ . ,(syscall-result syscall))
		    (@arg-defns@ . ,(format-arg-scm-defn-list (syscall-args syscall)))
		    (@scm-to-c-locals@ . ,(format-scm-to-c-locals (syscall-args syscall)))
		    (@c-arg-list@ . ,(format-c-arg-list (syscall-args syscall)))
		    (@scm-from-type@ . ,(convert-scm-from-type (syscall-result syscall)))
		    (@scm-to-c-locals-cleanup@ . ,(format-scm-to-c-locals-cleanup (syscall-args syscall)))))))
	    *syscalls*)

  (newline)
  (f #t "// Enable or disable syscall wrapping.\n")
  (newline)
  ;; { indented one space to not confuse emacs auto-indent
  (d "\
static SCM\n\
wsscm_enable_x (SCM syscalls)\n\
 {\n\
  while (! scm_is_null (syscalls))\n\
  {\n\
    SCM syscall = scm_car (syscalls);\n\
    SCM arg = scm_cadr (syscalls);\n\
\n\
    if (0)\n\
      ;\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "\
    else if (scm_is_eq (syscall, scm_from_latin1_symbol (\"@name@\")))\n\
    {\n\
      ws_wrap_@name@ = scm_is_true (arg) ? ws_c_wrap_@name@ : NULL;\n\
      wsscm_wrap_@name@ = arg;\n\
    }\n"
		  `((@name@ . ,(syscall-name syscall))))))
	    *syscalls*)
  (d "\
\n\
    syscalls = scm_cddr (syscalls);\n\
  }\n\
\n\
  return SCM_UNSPECIFIED;\n\
}\n")

  (d "// This is intended to be run with the current-module set to wrap-syscall.\n\n")
  (d "static void\n")
  (d "wrap_syscall_initialize_gen (void)\n")
  (d "{\n")
  (for-each (lambda (syscall)
	      (d (string-keyed-replace
		  "  scm_c_define_gsubr (\"real-@name@\", @nr-args@, 0, 0, wsscm_real_@name@);\n"
		  `((@name@ . ,(syscall-name syscall))
		    (@nr-args@ . ,(length (syscall-args syscall)))))))
	    *syscalls*)
  (newline)
  (d "  scm_c_define_gsubr (\"enable!\", 0, 0, 1, wsscm_enable_x);\n")
  (d "}\n")
)
