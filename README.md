# 開発環境

MSYS2 インストール
$ pacman -S mingw64/mingw-w64-x86_64-gcc
Windows の環境変数を編集から PATH に C:\msys64\mingw64\bin を追加

## cl-portaudio

portaudio.lisp
define-foreign-library で "portaudio_x64.dll" と ".dll" を付けないとエラーになる。

# c2ffi

```
pacman -S mingw-w64-x86_64-clang mingw-w64-x86_64-clang-tools-extra gcc cmake ninja
git clone https://github.com/rpav/c2ffi.git
# この時点で llvm-18.1.0 ブランチになっている
cd c2ffi
mkdir build
cd build
cmake -G Ninja ..
cmake --build .
```
