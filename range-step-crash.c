/* This testcase is part of wrap-syscall.

   Copyright 2002-2014 Free Software Foundation, Inc.

   This program is free software; you can redistribute it and/or modify
   it under the terms of the GNU General Public License as published by
   the Free Software Foundation; either version 3 of the License, or
   (at your option) any later version.

   This program is distributed in the hope that it will be useful,
   but WITHOUT ANY WARRANTY; without even the implied warranty of
   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
   GNU General Public License for more details.

   You should have received a copy of the GNU General Public License
   along with this program.  If not, see <http://www.gnu.org/licenses/>.
 
   This file is copied from gdb/testsuite/gdb.threads/schedlock.c.  */

#include <stdio.h>
#include <unistd.h>
#include <stdlib.h>
#include <pthread.h>
#include <signal.h>
#include <string.h>

#define NR_THREADS 2

unsigned int counters[NR_THREADS + 1];

/* Threads advance in a round-robin fashion.
   This is used to control the advancement.
   A thread advances when x == thread#.  */
pthread_mutex_t x_mutex;
int x;

sig_atomic_t sigusr1_seen;

void break_here (void) __attribute ((noinline));

int
break_here_before_inc (void)
{
  asm volatile ("");
  return 0;
}

void
break_here (void)
{
  asm volatile ("");
}

int
inc_x (int thread_nr)
{
  int result;

  pthread_mutex_lock (&x_mutex);
  if (x != thread_nr)
    result = -1;
  else
    {
      result = ++x;
      if (x >= NR_THREADS)
	x = 0;
    }
  pthread_mutex_unlock (&x_mutex);

  return result;
}

void *
thread_function (void *arg)
{
  int my_number = (long) arg;
  int *myp = (int *) &counters[my_number];
  static volatile int t;

  /* Don't run forever.  Run just short of it :)  */
  while (*myp > 0)
    {
      t = break_here_before_inc ();

      int y = inc_x (my_number);

      if (y >= 0)
	{
	  (*myp) ++; usleep (1);  /* Loop increment.  */
	  if (my_number == 0)
	    break_here ();
	}
      else
	{
	  usleep (1);
	}
    }

  pthread_exit (NULL);
}

static void
sigusr1_handler (int signo, siginfo_t *siginfo, void *exception)
{
  sigusr1_seen = 1;
}

int
main ()
{
  int res;
  pthread_t threads[NR_THREADS];
  void *thread_result;
  int i;
  struct sigaction sigact;

  memset (&sigact, 0, sizeof (sigact));
  sigact.sa_sigaction = sigusr1_handler;
  sigact.sa_flags = SA_RESTART | SA_SIGINFO;
  sigemptyset (&sigact.sa_mask);
  sigaction (SIGUSR1, &sigact, NULL);

  pthread_mutex_init (&x_mutex, NULL);

  for (i = 0; i < NR_THREADS; i++)
    {
      /* The call to usleep is so that when the watchpoint triggers,
	 the pc is still on the same line.  */
      counters[i] = 1; usleep (1); /* Init value.  */
    }

  for (i = 0; i < NR_THREADS; i++)
    {
      res = pthread_create (&threads[i],
			    NULL,
			    thread_function,
			    (void *) (intptr_t) i);
      if (res != 0)
	{
	  fprintf (stderr, "error in thread %d create\n", i);
	  abort ();
	}
    }

  //counters[i] = 1;
  //thread_function ((void *) i);

  for (i = 0; i < NR_THREADS; ++i)
    {
      res = pthread_join (threads[i], NULL);
      if (res != 0)
	{
	  fprintf (stderr, "error in thread %d join\n", i);
	  abort ();
	}
    }

  exit (EXIT_SUCCESS);
}
