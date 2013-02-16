#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
;;; fast sxml to xml converter using libxml2 (http://xmlsoft.org/)
;;;
;;; Copyright (C) 2013 Jens Thiele <karme@karme.de>
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
(define-module sxml2xml
  (use runtime-compile)
  (use file.util)
  (use gauche.process)
  (export make-xml-writer
          close-xml-writer
          xml-writer-write))

(select-module sxml2xml)

(compile-and-load
 `((inline-stub
    (declcode
     (.include <assert.h>)
     (.include <libxml/xmlwriter.h>)
     (.include <libxml/xmlsave.h>)
     "static ScmClass *scm_xml_writer_class = NULL;")

    (define-cfn xml-writer-print (h p::ScmPort* c::ScmWriteContext*)
      ::void :static
      (Scm_Printf p
                  "#<xml-writer @%p->%p>"
                  h
                  (SCM_FOREIGN_POINTER_REF xmlTextWriterPtr h)))

    (define-cfn xml-writer-cleanup (b) ::void :static
      (xmlFreeTextWriter (SCM_FOREIGN_POINTER_REF xmlTextWriterPtr b)))

    (define-cproc make-xml-writer (port-or-fd)
      ;; flush the output buffer if it is a port
      ;; todo:
      ;; ideally we would implement a libxml2 output buffer using
      ;; gauche's buffer
      (when (SCM_PORTP port-or-fd)
        (SCM_FLUSH port-or-fd))
      (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
        (return (Scm_MakeForeignPointer
                 scm_xml_writer_class (xmlNewTextWriter
                                       (xmlOutputBufferCreateFd fd NULL))))))
    
    (define-cproc close-xml-writer (b) ::<void>
      (unless (SCM_XTYPEP b scm_xml_writer_class) (SCM_TYPE_ERROR b "<xml-writer>"))
      (xmlFreeTextWriter (SCM_FOREIGN_POINTER_REF xmlTextWriterPtr b)))

    (define-cfn write-tree (tree writer::xmlTextWriterPtr) ::void :static
      ;;(Scm_Write (SCM_MAKE_BOOL (SCM_PAIRP tree)) (Scm_Stderr) SCM_WRITE_WRITE)
      (cond [(SCM_PAIRP tree)
             (if (SCM_SYMBOLP (SCM_CAR tree))
               (cond [(SCM_EQ (SCM_CAR tree) (SCM_INTERN "@"))
                      (for-each (lambda(x)
                                  (assert (SCM_PAIRP x))
                                  (assert (SCM_SYMBOLP (SCM_CAR x)))
                                  (assert (SCM_PAIRP (SCM_CDR x)))
                                  (assert (SCM_STRINGP (SCM_CADR x)))
                                  (xmlTextWriterWriteAttribute
                                   writer
                                   (SCM_STRING_CONST_CSTRING (SCM_SYMBOL_NAME (SCM_CAR x)))
                                   (SCM_STRING_CONST_CSTRING (SCM_CADR x))))
                                (SCM_CDR tree))]
                     [else
                      (xmlTextWriterStartElement writer
                                                 (SCM_STRING_CONST_CSTRING (SCM_SYMBOL_NAME (SCM_CAR tree))))
                      (for-each (lambda(x) (write-tree x writer)) (SCM_CDR tree))
                      (xmlTextWriterEndElement writer)])
               (for-each (lambda(x) (write-tree x writer)) (SCM_CDR tree)))]
            [(SCM_STRINGP tree)
             (xmlTextWriterWriteString writer (SCM_STRING_CONST_CSTRING tree))]
            ;; todo: else?
            ))
    
    (define-cproc xml-writer-write (b tree) ::<void>
      (unless (SCM_XTYPEP b scm_xml_writer_class) (SCM_TYPE_ERROR b "<xml-writer>"))
      (write-tree tree (SCM_FOREIGN_POINTER_REF xmlTextWriterPtr b)))
    
    (initcode (= scm_xml_writer_class (Scm_MakeForeignPointerClass
                                       (Scm_CurrentModule)
                                       "<xml-writer>" xml-writer-print xml-writer-cleanup
                                       SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
    ))
 '(make-xml-writer close-xml-writer xml-writer-write)
 :cflags (string-join (list (string-append "-I" (current-directory))
                            (process-output->string '(pkg-config libxml-2.0 --cflags))))
 :libs (process-output->string '(pkg-config libxml-2.0 --libs)))

(define make-xml-writer make-xml-writer)
(define xml-writer-write xml-writer-write)
(define close-xml-writer close-xml-writer)
