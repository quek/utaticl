# 開発環境

MSYS2 インストール
$ pacman -S mingw64/mingw-w64-x86_64-gcc
Windows の環境変数を編集から PATH に C:\msys64\mingw64\bin を追加

## ImGui バックエンド

```
pacman -S mingw64/mingw-w64-x86_64-cmake
pacman -S mingw64/mingw-w64-x86_64-SDL2
cd lib/cimgui/backend_test/example_sdl_opengl3
mkdir build
cd build
cmake -DSDL_PATH="" ..
cmake --build .
```

これでできた cimgui_sdl.dll をロードする。
この DLL は ImGui を含んでいるので cimgui.dll はいらない。

## cl-portaudio

portaudio.lisp
define-foreign-library で "portaudio_x64.dll" と ".dll" を付けないとエラーになる。
