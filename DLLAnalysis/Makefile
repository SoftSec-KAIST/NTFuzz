BUILDDIR=$(shell pwd)/build

all: DLLAnalysis

DLLAnalysis:
		@mkdir -p $(BUILDDIR)
		@dotnet build -c Debug -o $(BUILDDIR) src/DLLAnalysis.fsproj

clean:
		@dotnet clean -c Debug
		@rm -rf $(BUILDDIR)

.PHONY: DLLAnalysis all clean

