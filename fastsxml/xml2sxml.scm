#| -*- mode: scheme; coding: utf-8; -*- |#
;;;
;;; fast xml to sxml converter using libxml2 (http://xmlsoft.org/)
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
(define-module xml2sxml
  (use runtime-compile)
  (use file.util)
  (use gauche.process)
  (export make-xml-reader
          close-xml-reader
          xml-reader-read
          xml-reader-next
          xml-reader-node
          xml-reader-depth
          ))

(select-module xml2sxml)

(compile-and-load
 `((inline-stub
    (declcode
     (.include <assert.h>)
     (.include <libxml/xmlreader.h>)
     (.include <libxml/xmlsave.h>)
     "static ScmClass *scm_xml_reader_class = NULL;")

    (define-cfn xml-reader-print (h p::ScmPort* c::ScmWriteContext*)
      ::void :static
      (Scm_Printf p
                  "#<xml-reader @%p->%p>"
                  h
                  (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr h)))
    
    (define-cfn xml-reader-cleanup (b) ::void :static
      (xmlFreeTextReader (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b)))
    
    (define-cproc make-xml-reader (port-or-fd)
      ;; todo:
      ;; ideally we would implement a libxml2 input buffer using
      ;; gauche's buffer
      (let* ([fd::int (Scm_GetPortFd port-or-fd TRUE)])
        (return (Scm_MakeForeignPointer
                 scm_xml_reader_class (xmlNewTextReader
                                       (xmlParserInputBufferCreateFd fd XML_CHAR_ENCODING_UTF8)
                                       NULL)))))
    
    (define-cproc close-xml-reader (b) ::<void>
      (unless (SCM_XTYPEP b scm_xml_reader_class) (SCM_TYPE_ERROR b "<xml-reader>"))
      (xmlFreeTextReader (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b)))

    (define-cfn convertAttributes(a::xmlAttrPtr)
      (let* ((r (SCM_LIST1 (SCM_INTERN "@")))
             (co r))
        (while (!= a NULL)
          (SCM_SET_CDR co (SCM_LIST1 (SCM_LIST2 (SCM_INTERN (-> a name))
                                                (SCM_MAKE_STR_IMMUTABLE (-> a children content)))))
          (set! co (SCM_CDR co))
          (set! a (-> a next)))
        (return r)))

    (define-cfn convert (node::xmlNodePtr l::int)
      (let* ((r (SCM_LIST1 (SCM_INTERN (-> node name))))
             (co r))
        (when (-> node properties)
          (SCM_SET_CDR co (SCM_LIST1 (convertAttributes (-> node properties))))
          (set! co (SCM_CDR co)))
        (let* ((c::xmlNodePtr (-> node children)))
          (while c
            ;; todo: support others
            (cond [(== (-> c type) XML_ELEMENT_NODE)
                   (SCM_SET_CDR co (SCM_LIST1 (convert c (+ l 1))))
                   (set! co (SCM_CDR co))]
                  [(== (-> c type) XML_TEXT_NODE)
                   ;; optionally filter whitespace?!
                   (SCM_SET_CDR co (SCM_LIST1 (SCM_MAKE_STR_IMMUTABLE (-> c content))))
                   (set! co (SCM_CDR co))])
            (set! c (-> c next)))
          (return r))))
    
    (define-cproc xml-reader-node (b)
      (unless (SCM_XTYPEP b scm_xml_reader_class) (SCM_TYPE_ERROR b "<xml-reader>"))
      (let* ((reader::xmlTextReaderPtr (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b)))
        (if (== (xmlTextReaderNodeType reader) XML_READER_TYPE_ELEMENT)
          ;; todo: result only valid until next call?!
          (return (convert (xmlTextReaderExpand reader) 0))
          (return SCM_FALSE))))

    (define-cproc xml-reader-depth (b)
      (unless (SCM_XTYPEP b scm_xml_reader_class) (SCM_TYPE_ERROR b "<xml-reader>"))
      (return (SCM_MAKE_INT (xmlTextReaderDepth (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b)))))
      
    (define-cproc xml-reader-next (b)
      (unless (SCM_XTYPEP b scm_xml_reader_class) (SCM_TYPE_ERROR b "<xml-reader>"))
      (let* ((ret::int (xmlTextReaderNext (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b))))
        (cond [(== ret 1)
               (return SCM_TRUE)]
              [(== ret 0)
               (return SCM_FALSE)]
              [else
               (Scm_Error "parse error")])))

    (define-cproc xml-reader-read (b)
      (unless (SCM_XTYPEP b scm_xml_reader_class) (SCM_TYPE_ERROR b "<xml-reader>"))
      (let* ((ret::int (xmlTextReaderRead (SCM_FOREIGN_POINTER_REF xmlTextReaderPtr b))))
        (cond [(== ret 1)
               (return SCM_TRUE)]
              [(== ret 0)
               (return SCM_FALSE)]
              [else
               (Scm_Error "parse error")])))
    
    (initcode (= scm_xml_reader_class (Scm_MakeForeignPointerClass
                                       (Scm_CurrentModule)
                                       "<xml-reader>" xml-reader-print xml-reader-cleanup
                                       SCM_FOREIGN_POINTER_KEEP_IDENTITY)))
    ))
 '(make-xml-reader close-xml-reader xml-reader-node xml-reader-read xml-reader-next)
 :cflags (string-join (list (string-append "-I" (current-directory))
                            (process-output->string '(pkg-config libxml-2.0 --cflags))))
 :libs (process-output->string '(pkg-config libxml-2.0 --libs)))

(define make-xml-reader make-xml-reader)
(define xml-reader-read xml-reader-read)
(define xml-reader-next xml-reader-next)
(define xml-reader-node xml-reader-node)
(define close-xml-reader close-xml-reader)
