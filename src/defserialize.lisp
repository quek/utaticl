(in-package :dgw)

(defserialize neko neko-id name color)

(defserialize project bpm master-track loop-start loop-end loop-p)

(defserialize track
    (:list lanes :writer lane-add)
  (:list modules :writer module-add)
  (:list tracks :writer track-add-without-connect))

(defserialize lane (:list clips :writer clip-add))

(defserialize time-thing time duration)

(defserialize note key channel velocity)

(defserialize clip seq)

(defserialize clip-note)

(defserialize seq-note notes)

(defserialize plugin-info id name path file-write-date)

(defserialize module connections)

(defserialize connection (:ref from) (:ref to) from-bus-index to-bus-index)

(defserialize param id value)
