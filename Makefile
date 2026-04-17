.PHONY: ion-test-suite
ion-test-suite:
	ION_TESTS_ROOT=ion-tests/iontestdata dune exec test/test_ion_suite.exe