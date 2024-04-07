(in-package :dgw)

(let* ((factory (vst3::get-plugin-factory "c:/Program Files/Common Files/VST3/Dexed.vst3")))
  (vst3::create-component factory))
;;⇒ #<VST3-FFI::STEINBERG-VST-ICOMPONENT {1004590EB3}>

