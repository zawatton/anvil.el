.PHONY: test test-all lint byte-compile clean sync-toml-filters check-toml-filters-sync

# Pin the temp directory for every recipe below.
#
# Two separate failures motivated this, and they need `:=', not `?='.
#
#   - make on MSYS strips TEMP/TMP from the environment, so
#     `make-temp-file' in the subprocess falls back to `c:/' (unwritable)
#     and every test touching a temp file fails.  `?=' already covered
#     this one: the variable was unset.
#
#   - A TMPDIR that is *set* but points somewhere the indexer excludes
#     is worse, because `?=' leaves it alone.  `anvil-semantic-exclude-
#     patterns' skips "/\.cache/", so a session whose TMPDIR is
#     ~/.cache/tmp makes every test corpus built by `make-temp-file'
#     invisible: "indexed 0 chunk(s) from 0 file(s)", and 38 tests across
#     org-index / semantic / fusion-ask / new-tools go red against a
#     tree that is actually green (measured 2026-09-19).
#
# The tests want a neutral temp dir, not the caller's.  `:=' gives them
# one in both cases.
export TMPDIR := /tmp
export TEMP   := /tmp
export TMP    := /tmp

test:
	eask test ert tests/anvil-test.el

test-all:
	emacs --batch --eval '(setq load-prefer-newer t)' -L . -l anvil-dev -f anvil-dev-test-run-all-batch

lint:
	eask lint package
	eask lint checkdoc

byte-compile:
	eask compile

clean:
	eask clean all

## Sync rtk-derived TOML filters -> anvil-shell-filter-builtin.el
sync-toml-filters:
	python3 scripts/sync-rtk-filters.py

check-toml-filters-sync:
	python3 scripts/sync-rtk-filters.py --check
