/* C side of the wrap-syscall Guile module.

   This file is part of wrap-syscall.

   Copyright 2014 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.  */

#include <errno.h>
#include "libguile.h"
#include "ws.h"

// Struct to marshall args through gdbscm_with_catch.

struct with_catch_data
{
  scm_t_catch_body func;
  void *data;
  scm_t_catch_handler unwind_handler;
  scm_t_catch_handler pre_unwind_handler;

  SCM stack;
  SCM catch_result;
};

// A "pre-unwind handler" to scm_c_catch that prints the exception.

static SCM
wsscm_printing_pre_unwind_handler (void *data, SCM key, SCM args)
{
  SCM stack = scm_make_stack (SCM_BOOL_T, scm_list_1 (scm_from_int (2)));
  SCM error_port = scm_current_error_port ();

  // FIXME: for now
  scm_puts ("Exception caught:\n", error_port);
  scm_display (key, error_port);
  scm_puts (" ", error_port);
  scm_display (args, error_port);
  scm_newline (error_port);

  return SCM_UNSPECIFIED;
}

// A no-op unwind handler.

static SCM
wsscm_nop_unwind_handler (void *data, SCM key, SCM args)
{
  return SCM_BOOL_F;
}

/* Ugh. :-(
   Guile doesn't export scm_i_with_continuation_barrier which is exactly
   what we need.  To cope, have our own wrapper around scm_c_catch and
   pass this as the "body" argument to scm_c_with_continuation_barrier.
   Darn darn darn.  */

static void*
wsscm_with_catch (void *data)
{
  struct with_catch_data *d = data;

  d->catch_result
    = scm_c_catch (SCM_BOOL_T,
		   d->func, d->data,
		   d->unwind_handler, d,
		   d->pre_unwind_handler, d);

  return NULL;
}

/* A wrapper of scm_with_guile for use by the safe call/apply routines
   in this file.  If there is an exception it is printed and the result
   is #f.  */

static SCM
wsscm_call_guile (SCM (*func) (void *), void *data)
{
  struct with_catch_data catch_data;

  catch_data.func = func;
  catch_data.data = data;
  catch_data.unwind_handler = wsscm_nop_unwind_handler;
  catch_data.pre_unwind_handler = wsscm_printing_pre_unwind_handler;
  catch_data.stack = SCM_BOOL_F;
  catch_data.catch_result = SCM_BOOL_F;

#if 0
  scm_c_with_continuation_barrier (wsscm_with_catch, &catch_data);
#else
  scm_with_guile (wsscm_with_catch, &catch_data);
#endif

  return catch_data.catch_result;
}

static SCM
wsscm_call_0_body (void *argsp)
{
  SCM *args = argsp;

  return scm_call_0 (args[0]);
}

SCM
wsscm_safe_call_0 (SCM proc)
{
  SCM args[] = { proc };

  return wsscm_call_guile (wsscm_call_0_body, args);
}

static SCM
wsscm_call_1_body (void *argsp)
{
  SCM *args = argsp;

  return scm_call_1 (args[0], args[1]);
}

SCM
wsscm_safe_call_1 (SCM proc, SCM arg1)
{
  SCM args[] = { proc, arg1 };

  return wsscm_call_guile (wsscm_call_1_body, args);
}

static SCM
wsscm_call_2_body (void *argsp)
{
  SCM *args = argsp;

  return scm_call_2 (args[0], args[1], args[2]);
}

SCM
wsscm_safe_call_2 (SCM proc, SCM arg1, SCM arg2)
{
  SCM args[] = { proc, arg1, arg2 };

  return wsscm_call_guile (wsscm_call_2_body, args);
}

static SCM
wsscm_call_3_body (void *argsp)
{
  SCM *args = argsp;

  return scm_call_3 (args[0], args[1], args[2], args[3]);
}

SCM
wsscm_safe_call_3 (SCM proc, SCM arg1, SCM arg2, SCM arg3)
{
  SCM args[] = { proc, arg1, arg2, arg3 };

  return wsscm_call_guile (wsscm_call_3_body, args);
}

static SCM
wsscm_call_4_body (void *argsp)
{
  SCM *args = argsp;

  return scm_call_4 (args[0], args[1], args[2], args[3], args[4]);
}

SCM
wsscm_safe_call_4 (SCM proc, SCM arg1, SCM arg2, SCM arg3, SCM arg4)
{
  SCM args[] = { proc, arg1, arg2, arg3, arg4 };

  return wsscm_call_guile (wsscm_call_4_body, args);
}

static SCM
ws_scm_from_void_ptr (void *ptr)
{
  return scm_from_uintmax ((uintptr_t) ptr);
}

static SCM
ws_scm_from_const_void_ptr (const void *ptr)
{
  return scm_from_uintmax ((uintptr_t) ptr);
}

static void*
ws_scm_to_void_ptr (SCM ptr)
{
  return (void*) scm_to_uintmax (ptr);
}

static const void*
ws_scm_to_const_void_ptr (SCM ptr)
{
  return (const void*) scm_to_uintmax (ptr);
}

#include "ws-gen.c"

static SCM
wsscm_get_wait_status (SCM stat_ptr)
{
  int *c_stat_ptr = (int*) scm_to_uintmax (stat_ptr);

  return scm_from_int (*c_stat_ptr);
}

static SCM
wsscm_set_wait_status_x (SCM stat_ptr, SCM new_value)
{
  int *c_stat_ptr = (int*) scm_to_uintmax (stat_ptr);
  int c_new_value = scm_to_int (new_value);

  *c_stat_ptr = c_new_value;

  return SCM_UNSPECIFIED;
}

// This is intended to be run with the current-module set to wrap-syscall.

void
wrap_syscall_initialize (void)
{
  wrap_syscall_initialize_gen ();

  scm_c_define_gsubr ("get-wait-status", 1, 0, 0, wsscm_get_wait_status);
  scm_c_define_gsubr ("set-wait-status!", 2, 0, 0, wsscm_set_wait_status_x);
}
