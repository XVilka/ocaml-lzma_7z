mkfile_path := $(abspath $(lastword $(MAKEFILE_LIST)))
mkfile_dir := $(notdir $(pathsubst %/,%,$(dir ($mkfile_path))))

PROG = lzma
CXX = g++
CC = gcc
AR = ar
RANLIB = ranlib
LIB =
RM = rm -f
CFLAGS = -c -O2 -Wall -D_7ZIP_ST -fPIC

OBJS = \
  Alloc.o \
  LzFind.o \
  LzmaDec.o \
  LzmaEnc.o \
  7zFile.o \
  7zStream.o \

all: liblzma_7z.so liblzma_7z.a

liblzma_7z.so: $(OBJS)
	$(CC) -fPIC -shared -o liblzma_7z.so $(OBJS) $(LIB) $(LDFLAGS)

liblzma_7z.a: $(OBJS)
	$(AR) rcs liblzma_7z.a $(OBJS)
	$(RANLIB) liblzma_7z.a

Alloc.o: Alloc.c
	$(CC) $(CFLAGS) Alloc.c

LzFind.o: LzFind.c
	$(CC) $(CFLAGS) LzFind.c

LzmaDec.o: LzmaDec.c
	$(CC) $(CFLAGS) LzmaDec.c

LzmaEnc.o: LzmaEnc.c
	$(CC) $(CFLAGS) LzmaEnc.c

7zFile.o: 7zFile.c
	$(CC) $(CFLAGS) 7zFile.c

7zStream.o: 7zStream.c
	$(CC) $(CFLAGS) 7zStream.c

clean:
	-$(RM) $(OBJS)
