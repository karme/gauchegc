#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
;;; hack to make some gc functions available in scheme
;;;
;;; Copyright (C) 2010-2012 Jens Thiele <karme@karme.de>
;;;
;;; This program is free software: you can redistribute it and/or modify
;;; it under the terms of the GNU General Public License as published by
;;; the Free Software Foundation, either version 3 of the License, or
;;; (at your option) any later version.
;;;
;;; This program is distributed in the hope that it will be useful,
;;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;; GNU General Public License for more details.
;;;
;;; You should have received a copy of the GNU General Public License
;;; along with this program.  If not, see <http://www.gnu.org/licenses/>.
;;;

(define-module gc-hack
  (use runtime-compile)
  (export gc-set-warn-proc
          gc-get-gc-no
	  gc-set-on-collection-event))

(select-module gc-hack)

(compile-and-load
 `((inline-stub
    (declcode
     (.include "gc.h")
     "static ScmObj _callback;"
     "static void my_warn_proc(char *msg, GC_word arg) {"
     "Scm_ApplyRec2(_callback,SCM_MAKE_STR(msg),Scm_MakeIntegerU64(arg));"
     "}"
     "static ScmObj _event_callback;"
     "static void my_event_proc(GC_EventType e) {"
     "if (e==GC_EVENT_START) Scm_ApplyRec0(_event_callback);"
     "}"
     )
    (define-cproc gc-set-warn-proc (callback::<procedure>)
      (set! _callback (SCM_OBJ callback))
      (GC_set_warn_proc my_warn_proc))
    (define-cproc gc-get-gc-no ()
      (result (SCM_MAKE_INT GC_gc_no))
      ;; newer gauche
      ;;(result (SCM_MAKE_INT (GC_call_with_alloc_lock (cast GC_fn_type GC_get_gc_no) 0)))
      )
    (define-cproc gc-set-on-collection-event (callback::<procedure>)
      (set! _event_callback (SCM_OBJ callback))
      (GC_set_on_collection_event my_event_proc))
    ))
 '(gc-set-warn-proc gc-get-gc-no gc-set-on-collection-event)
 :cppflags "-Wno-deprecated-declarations")
