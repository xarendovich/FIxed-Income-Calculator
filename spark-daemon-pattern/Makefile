# Spark daemon pattern: developer entry points. Standard library only; jsonschema is optional
# and only used by the schema-agreement tests (CI installs it).
PY ?= python3
REV ?= r3
SPARK = $(PY) -I -B bin/spark-daemon
EXAMPLES = $(sort $(dir $(wildcard examples/*/manifest.json)))

.PHONY: help test contract check-contract precheck-examples validate-examples battery-examples evidence clean

help:
	@echo "make test               self-tests (about 1 minute)"
	@echo "make contract           regenerate contract/*.json from the enforced rules"
	@echo "make check-contract     fail if contract/*.json is stale"
	@echo "make precheck-examples  static checks on every reference daemon (milliseconds)"
	@echo "make validate-examples  static checks plus a short confined run (about 1 s each)"
	@echo "make battery-examples   full battery on every reference daemon (about 15 s each)"
	@echo "make evidence           battery reports and self-test output into evidence/\$$(REV)/"

test:
	$(PY) -B -m unittest discover -s tests -t tests

contract:
	$(SPARK) describe > contract/daemon-contract.json
	$(SPARK) schema > contract/manifest.schema.json
	@echo "If contract_sha256 changed, bump CONTRACT_VERSION and add it to contract/versions.json."

check-contract:
	@$(SPARK) describe | diff -u contract/daemon-contract.json - && \
	 $(SPARK) schema | diff -u contract/manifest.schema.json - && echo "contract up to date"

precheck-examples:
	@set -e; for d in $(EXAMPLES); do \
	  $(SPARK) precheck --manifest $${d}manifest.json > /dev/null 2>&1 && echo "$$d PASS" || { echo "$$d FAIL"; exit 1; }; done

validate-examples:
	@set -e; for d in $(EXAMPLES); do \
	  $(SPARK) validate --manifest $${d}manifest.json > /dev/null 2>&1 && echo "$$d PASS" || { echo "$$d FAIL"; exit 1; }; done

battery-examples:
	@set -e; for d in $(EXAMPLES); do \
	  out=$$($(SPARK) battery --manifest $${d}manifest.json) || { echo "$$out"; echo "$$d FAIL"; exit 1; }; \
	  echo "$$d $$(echo "$$out" | tail -1)"; done

evidence:
	@set -e; out=evidence/$(REV); mkdir -p $$out; \
	for d in $(EXAMPLES); do n=$$(basename $$d); rm -rf battery-runs/$$n; \
	  $(SPARK) battery --manifest $${d}manifest.json --workdir battery-runs/$$n > $$out/battery-$$n.txt; \
	  cp battery-runs/$$n/battery-report.json $$out/battery-report-$$n.json; done; \
	$(PY) -B -m unittest discover -s tests -t tests -v > $$out/selftests-$$($(PY) -c 'import sys;print("python%d.%d"%sys.version_info[:2])').txt 2>&1

clean:
	rm -rf battery-runs
	find . -name __pycache__ -prune -exec rm -rf {} +
