# Writing tests

You can write test modules like the following:

```
./
├── README.md
├── sesterl.yaml
├── rebar.config
├── rebar.lock
├── src/
│   └── Adder.sest
└── test/
    └── AdderTests.sest
```

`sesterl.yaml`:

```
package: "adder"
language: "v0.2.0"
source_directories:
  - "./src"
main_module: "Adder"
test_directories:
  - "./test"
```

`src/Adder.sest`:

```
module Adder = struct

  val add(m, n) = m + n

end
```

`test/AdderTests.sest`:

```
import Adder

module AdderTests = #[test] struct

  #[test]
  val adder_test() =
    Testing.it("42 plus 57 equals 99", fun() ->
      assert Testing.equal(
        -expect 99,
        -got    Adder.add(42, 57),
      )
    end)

end
```

The following makes the test run:

```
$ sesterl config ./
$ rebar3 sesterl test
```
