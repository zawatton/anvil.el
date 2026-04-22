.PHONY: test test-all lint byte-compile clean

# make on MSYS strips TEMP/TMP from the environment, which makes
# `make-temp-file' in the subprocess fall back to `c:/' (unwritable)
# and fail every test that touches a temp file.  Force sane defaults.
export TMPDIR ?= /tmp
export TEMP   ?= /tmp
export TMP    ?= /tmp

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
