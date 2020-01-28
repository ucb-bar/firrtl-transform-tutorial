.PHONY: test test-partialize-connects test-partialize-and-remove test-errors test-resilient test-resilient-errors

test:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator'

test-partialize-connects:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator -fct gcd.tools.PartializeConnects'

test-partialize-and-remove:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator -fct gcd.tools.PartializeConnects gcd.tools.RemoveTrivialPartialConnects'

test-errors:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator -fct gcd.tools.ResilientRegisterFlow -faf src/main/resources/gcd/SoftErrorAnnos.json'

test-resilient:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator -fct gcd.tools.ResilientRegisterFlow -faf src/main/resources/gcd/ResilientRegisterAnnos.json'

test-resilient-errors:
	sbt 'test:runMain gcd.GCDMain --backend-name verilator -fct gcd.tools.ResilientRegisterFlow -faf src/main/resources/gcd/ResilientRegisterAnnos.json  -faf src/main/resources/gcd/SoftErrorAnnos.json'

