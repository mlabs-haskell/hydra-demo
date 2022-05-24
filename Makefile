FOURMOLU_EXTENSIONS := -o -XTypeApplications -o -XTemplateHaskell -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

NIX_SOURCES := $(shell find * -not -path 'dist-newstyle/*' -iname '*.nix')
CABAL_SOURCES := $(shell find * -not -path 'dist-newstyle/*' -iname '*.cabal')
HASKELL_SOURCES := $(shell find app src test -iname '*.hs')

check_all: format_check lint cabalfmt_check nixpkgsfmt_check

format_all: format nixpkgsfmt cabalfmt

format:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode inplace --check-idempotence $(HASKELL_SOURCES)

format_check:
	fourmolu $(FOURMOLU_EXTENSIONS) --mode check --check-idempotence $(HASKELL_SOURCES)

nixpkgsfmt:
	nixpkgs-fmt $(NIX_SOURCES)

nixpkgsfmt_check:
	nixpkgs-fmt --check $(NIX_SOURCES)

cabalfmt:
	cabal-fmt --inplace $(CABAL_SOURCES)

cabalfmt_check:
	cabal-fmt --check $(CABAL_SOURCES)

lint:
	hlint $(HASKELL_SOURCES)
