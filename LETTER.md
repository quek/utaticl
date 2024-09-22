# 不安定 落ちる

``` common-lisp
(loop while (.play-p (car (.projects *app*)))
      do (audio-loop))
```

これずっと動くので vst3 なしの audio-loop は問題なさそう。
SynthMaster 3, F-em, MSoundFactory をロードしてみてもずっと動く。

PortAudio への書き出しとかに問題がある？
