echo "starting"
echo "${src}"

cd $src/unzip60

# iconv patch
patch -Np1 -i $src/unzip60-alt-iconv-utf8.patch

# set CFLAGS -- from Debian
export CFLAGS="$CFLAGS -D_FILE_OFFSET_BITS=64 -DACORN_FTYPE_NFS \
-DWILD_STOP_AT_DIR -DLARGE_FILE_SUPPORT -DUNICODE_SUPPORT \
-DUNICODE_WCHAR -DUTF8_MAYBE_NATIVE -DNO_LCHMOD -DDATE_FORMAT=DF_YMD \
-DUSE_BZIP2 -DNATIVE"

# make -- from Debian
make -f ./unix/Makefile prefix=/data/vap/unzip-iconv linux
