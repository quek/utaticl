(in-package :utaticl.core)

(defserialize neko neko-id name color)

(defserialize project bpm loop-start loop-end loop-p
  master-track sceen-matrix)

(defserialize track (:ref parent)
    (:list modules :writer module-add)
  (:list lanes :writer lane-add) ;automation-module があるので modules の後に
  (:list tracks :writer track-add-without-connect))

(defserialize lane (:list clips :writer clip-add)
  (:ref automation-module) automation-param-id)

(defserialize time-thing time duration)

(defserialize note key channel velocity)

(defserialize clip offset-start seq (:ref lane) (:ref sceen))

(defserialize clip-note)

(defserialize seq-audio samples)
(defserialize sample nchannels sample-rate data-original duration-original path)

(defserialize seq-automation (:list points :writer automation-point-add))

(defserialize automation-point value)

(defserialize seq-note notes)

(defserialize plugin-info id name path file-write-date)

(defserialize module id connections)

(defserialize connection (:ref from) (:ref to) from-bus-index to-bus-index)

(defserialize sceen-matrix (:list sceens :writer sceen-add))

(defserialize sceen height (:hash clips :writer (lambda (sceen lane clip)
                                                  (clip-add sceen clip :lane lane))))
