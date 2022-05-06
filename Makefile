hoogle:
	hoogle server --local --port=8070 > /dev/null &

# Source dirs to run fourmolu on
FORMAT_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.hs' )

# Extensions we need to tell fourmolu about
FORMAT_EXTENSIONS := -o -XTemplateHaskell -o -XTypeApplications -o -XImportQualifiedPost -o -XPatternSynonyms -o -fplugin=RecordDotPreprocessor

# Run fourmolu formatter
format:
	fourmolu --mode inplace --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Check formatting (without making changes)
format_check:
	fourmolu --mode check --check-idempotence $(FORMAT_EXTENSIONS) $(FORMAT_SOURCES)

# Nix files to format
NIX_SOURCES := $(shell git ls-tree -r HEAD --full-tree --name-only | grep -E '.*\.nix' )

nixfmt:
	nixfmt $(NIX_SOURCES)

nixfmt_check:
	nixfmt --check $(NIX_SOURCES)

lint:
	hlint $(FORMAT_SOURCES)
