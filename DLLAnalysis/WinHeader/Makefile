BUILDDIR=$(shell pwd)/build

all: WinHeader

WinHeader:
		@dotnet build -c Debug -o $(BUILDDIR) src/WinHeader.fsproj

clean:
		@dotnet clean -c Debug
		rm -rf $(BUILDDIR)

.PHONY: WinHeader all clean

