# FORTRAT — Makefile
# Requires: gfortran, libcurl-dev, json-fortran
#
# json-fortran: clone and build first:
#   git clone https://github.com/jacobwilliams/json-fortran vendor/json-fortran
#   cd vendor/json-fortran && mkdir build && cd build && cmake .. && make
#
# Then: make

CC       = gcc
FC       = gfortran
FFLAGS   = -O2 -std=f2018 -Wall -fimplicit-none
CFLAGS   = -O2 -Wall
LIBS     = -lcurl
JSONF    = vendor/json-fortran/build/lib/libjsonfortran.a
JSONINC  = -I vendor/json-fortran/build/include

SRC = src/types.f90 \
      src/tui.f90 \
      src/simulate.f90 \
      src/render.f90 \
      src/fetch.f90 \
      src/lexparse.f90 \
      src/main.f90

.PHONY: all clean

all: fortrat

src/tui_helper.o: src/tui_helper.c
	$(CC) $(CFLAGS) -c src/tui_helper.c -o src/tui_helper.o

fortrat: $(SRC) $(JSONF) src/tui_helper.o
	$(FC) $(FFLAGS) $(JSONINC) -o fortrat $(SRC) src/tui_helper.o $(JSONF) $(LIBS)

$(JSONF):
	@echo "Building json-fortran..."
	@mkdir -p vendor
	@if [ ! -d vendor/json-fortran ]; then \
	  git clone --depth=1 https://github.com/jacobwilliams/json-fortran vendor/json-fortran; \
	fi
	@mkdir -p vendor/json-fortran/build
	@cd vendor/json-fortran/build && cmake .. -DSKIP_DOC_GEN=TRUE && $(MAKE) --no-print-directory

clean:
	rm -f fortrat *.mod *.o src/*.o

distclean: clean
	rm -rf vendor/json-fortran/build
