# パッケージ 名前空間 ファイル分け ファイル構成

ファイル内ローカルが欲しい。

* ファイルごとにパッケージを作る。
  * シンボルのエクスポート管理がめんどくさい。
* ファイルごととのプレフィクスを付ける。
  * 奇麗じゃない。



# 不安定 落ちる

``` common-lisp
(cffi:with-foreign-object (buffer :float 1024)
  (loop while (.play-p (car (.projects *app*)))
        do (audio-loop buffer)
           (sleep .01)))
```

これずっと動くので vst3 なしの audio-loop は問題なさそう。
SynthMaster 3, F-em, MSoundFactory をロードしてみてもずっと動く。

PortAudio への書き出しとかに問題がある？

``` common-lisp
(let ((n 0))
 (defun foo ()
   (print (list (incf n) (.steady-time *app*) (.play-start (car (.projects *app*)))))))

(sb-thread:make-thread
 (lambda ()
   (sb-int:with-float-traps-masked (:invalid :inexact :overflow :divide-by-zero)
     (utaticl::with-ole
       (with-thraed-pool
         (setf *app* (make-instance 'app :backend :sdl-vulkan))
         (unwind-protect
              (progn
                (audio-thread-start *app*)
                (setf (.play-p (car (.projects *app*))) t)
                (loop do (sleep 1) (foo)))
           (utaticl.core:terminate utaticl.core:*app*)))))
   )
 :name "UTATICL")
```

これもずっと動いた。

UI とオーディオスレッドの両方が動くとだめ？

``` common-lisp
1 file changed, 2 insertions(+), 3 deletions(-)
src/audio-device.lisp | 5 ++---

modified   src/audio-device.lisp
@@ -134,9 +134,8 @@
            (ignore input-buffer time-info status-flags user-data
                    frame-per-buffer))
   ;; sb-sys:without-gcing しなくてもたいして変わらない気もする
-  (sb-sys:without-gcing
-    (audio-loop output-buffer)
-    0))
+  (audio-loop output-buffer)
+  0)
 
 (defun statistic-enter (self)
   (let* ((now (get-internal-real-time))
```

sb-sys:without-gcing をやめたら安定した。
