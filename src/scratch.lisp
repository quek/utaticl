(in-package :dgw)

(let* ((x (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3"))
       (factory3 (vst3::query-interface x vst3::*iplugin-factory3-iid*)))
  (loop for i below (vst3::count-classes factory3)
        collect (vst3::get-class-info-unicode factory3 0)))
;;⇒ (#<VST3::PCLASS-INFO-W {1004115EE3}> #<VST3::PCLASS-INFO-W {1004116303}>)

