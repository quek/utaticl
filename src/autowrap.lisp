(in-package :dgw.ffi)

(autowrap:c-include '(dgw autowrap-spec "cimgui.h")
                    :spec-path '(dgw autowrap-spec)
                    :exclude-sources ("/usr/include/")
                    :include-sources ("cimgui.h")
                    :exclude-definitions ("_inline$"
					  "^_mm_"
					  "^__"
					  "va_list"
					  "_gnuc_va_list")
                    )
