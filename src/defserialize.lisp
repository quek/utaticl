(in-package :utaticl.core)

(defserialize neko neko-id name color)

(defserialize project bpm loop-start loop-end loop-p
  master-track sceen-matrix)

(defserialize track
    (:list lanes :writer lane-add)
  (:list modules :writer module-add)
  (:list tracks :writer track-add-without-connect))

(defserialize lane (:list clips :writer clip-add))

(defserialize time-thing time duration)

(defserialize note key channel velocity)

(defserialize clip seq (:ref lane) (:ref sceen))

(defserialize clip-note)

(defserialize seq-note notes)

(defserialize seq-audio nchannels sample-rate data-original duration-original path)

(defserialize plugin-info id name path file-write-date)

(defserialize module connections)

(defserialize connection (:ref from) (:ref to) from-bus-index to-bus-index)

(defserialize param id value)

(defserialize sceen-matrix (:list sceens :writer sceen-add))

(defserialize sceen height (:hash clips :writer (lambda (sceen lane clip)
                                                  (clip-add sceen clip :lane lane))))
