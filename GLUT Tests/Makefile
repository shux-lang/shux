ifeq ($(OS),Windows_NT)
	MSG := Setting graphics library paths for Windows / Cygwin
    INCLUDE_PATH = -I/usr/include/opengl
	LIBRARY_PATH = -L/usr/lib/w32api
	OPENGL_LIBS  = -lglut32 -lglu32 -lopengl32
else
    UNAME_S := $(shell uname -s)
    ifeq ($(UNAME_S),Linux)
		MSG := Setting graphics library paths for Linux
		INCLUDE_PATH =
		LIBRARY_PATH =
		OPENGL_LIBS  = -lglut -lGL -lGLU -lX11
    endif
    ifeq ($(UNAME_S),Darwin)
		MSG := Setting graphics library paths for macOS
		INCLUDE_PATH = -I/usr/local/include/
		LIBRARY_PATH = -L/usr/local/lib/
		OPENGL_LIBS  = -framework OpenGL -framework GLUT
    endif
endif

TARGET = main.native
CC = clang++
LLCC = llc
CFLAGS = -S -emit-llvm $(INCLUDE_PATH)
LFLAGS = $(LIBRARY_PATH)
LIBS = $(OPENGL_LIBS)

OBJS = main.s main.ll

default: msg $(TARGET)

all: clean msg $(TARGET)

.PHONY:
msg:
	$(info $(MSG))

$(TARGET): main.s
	clang++ $(LFLAGS) main.s $(LIBS) -o $(TARGET)

main.s: main.ll
	$(LLCC) main.ll

main.ll: main.cpp
	$(CC) $(CFLAGS) main.cpp

clean:
	rm -f $(OBJS)
	rm -f $(TARGET)
	rm -f $(TARGET).exe