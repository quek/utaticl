# 開発環境

MSYS2 インストール
$ pacman -S mingw64/mingw-w64-x86_64-gcc
Windows の環境変数を編集から PATH に C:\msys64\mingw64\bin を追加

## cl-portaudio

portaudio.lisp
define-foreign-library で "portaudio_x64.dll" と ".dll" を付けないとエラーになる。
