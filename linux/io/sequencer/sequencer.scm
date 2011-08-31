#| -*- mode: scheme; coding: utf-8; -*- |#
(define-module sequencer
  (use c-wrapper)
  (use gauche.sequence)
  (use gauche.uvector)
  (use srfi-1)
  (use util.list)
  (use gauche.process)
  (export seq-open
          seq-close
          seq-make-port
          seq-note-on
          seq-note-off
          seq-program-change
          seq-flush))

(select-module sequencer)

(define (assert x)
  (if (not x)
      (error "assertion failed")))

(c-load '("alsa/asoundlib.h" "alsa/seq.h")
	:import (list (lambda (header sym)
			(#/\/alsa\/.*\.h$/ header)))
	;;:libs-cmd "pkg-config --libs alsa"
        ;; todo: multi-arch trouble
        :libs-cmd "sh -c 'echo -L/usr/lib/$(dpkg-architecture -qDEB_HOST_MULTIARCH) $(pkg-config alsa --libs)'"
        :module #f)

(define (seq-open . args)
  (let-optionals* args ((name "sgachine"))
    (let ((seq_handle (make (ptr <snd_seq_t>))))
      (snd_seq_open (ptr seq_handle) "hw" SND_SEQ_OPEN_OUTPUT 0)
      (snd_seq_set_client_name seq_handle name)
      seq_handle)))

(define (seq-flush seq_handle)
  (when seq_handle
    (snd_seq_drain_output seq_handle)))

(define (seq-close seq_handle)
  (when seq_handle
    (seq-flush seq_handle)
    (snd_seq_close seq_handle)))

(define (seq-send-event seq_handle port ev flush)
  (when seq_handle
    (snd_seq_ev_set_direct (ptr ev))
    (snd_seq_ev_set_source (ptr ev) port)
    (snd_seq_ev_set_dest (ptr ev) SND_SEQ_ADDRESS_SUBSCRIBERS 0)
    (snd_seq_event_output seq_handle (ptr ev))
    (if flush
      (snd_seq_drain_output seq_handle))))

(define (seq-note-on seq_handle port channel note vel)
  (when seq_handle
    (let1 ev (make <snd_seq_event_t>)
      (snd_seq_ev_set_noteon (ptr ev) channel note vel)
      (seq-send-event seq_handle port ev #f))))

(define (seq-note-off seq_handle port channel note vel)
  (when seq_handle
    (let1 ev (make <snd_seq_event_t>)
      (snd_seq_ev_set_noteoff (ptr ev) channel note vel)
      (seq-send-event seq_handle port ev #f))))

(define (seq-program-change seq_handle port channel pid)
  (assert (and (>= pid 0) (<= pid 127)))
  (let1 ev (make <snd_seq_event_t>)
    (snd_seq_ev_set_pgmchange (ptr ev) channel pid)
    (seq-send-event seq_handle port ev #t)))

(define (control seq_handle port channel type val)
  (assert (and (>= val 0) (<= val 127)))
  (let1 ev (make <snd_seq_event_t>)
	(snd_seq_ev_set_controller (ptr ev) channel type val)
	(seq-send-event seq_handle port ev #t)))

  
(define (note seq_handle port channel n vel)
  (note-on seq_handle port channel n vel)
  (note-on seq_handle port (+ channel 10) (+ n 3) vel)
  (for-each
   (lambda(w)
     ;; s.a. asoundef.h
     (control seq_handle port channel        MIDI_CTL_MSB_MODWHEEL w)
     (control seq_handle port (+ channel 10) MIDI_CTL_MSB_MODWHEEL w)
     (control seq_handle port channel        MIDI_CTL_MSB_PAN w)
     (control seq_handle port (+ channel 10) MIDI_CTL_MSB_PAN w)
     (snd_seq_drain_output seq_handle)
     (sys-nanosleep 100000000))
   (iota (quotient 127 8) 0 8))
  (note-off seq_handle port channel n vel)
  (note-off seq_handle port (+ channel 10) (+ n 3) vel))

(define (seq-make-port seq_handle name . args)
  (if (not seq_handle)
    #f
    (let-optionals* args ((dest_id #f))
      (let* ((port (snd_seq_create_simple_port seq_handle
                                               name
                                               (logior 
                                                SND_SEQ_PORT_CAP_READ 
                                                SND_SEQ_PORT_CAP_SUBS_READ) 
                                               (logior 
                                                SND_SEQ_PORT_TYPE_MIDI_GENERIC
                                                SND_SEQ_PORT_TYPE_APPLICATION)))
             (client #`",(snd_seq_client_id seq_handle):,|port|"))
        (if dest_id
          (begin
            (format #t "connecting ~d to ~d ..." client dest_id)
            (run-process `(aconnect ,client ,dest_id) :wait #t)
            (format #t " ok\n"))
          (begin
            (format #t "please connect ~d\n" client)))
        port))))
