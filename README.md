# imperator
See the `examples` folder to get a sense of how it works.

## Building / Running
You'll just need the [Haskell `stack`](https://docs.haskellstack.org/en/stable/README/)
```
git clone https://github.com/iostat/imperator.git
cd imperator
stack setup
stack install
stack exec imperator -- examples/some_file.dsl # the double dash is important, stack quirk.
```

## Quirks
1. if you miss semicolons separating your statements, they probably wont run with no parser error given
2. DateTimes are ISO 8601
  * i.e. YYYY-MM-DD or YYYY-MM-DD*T*HH:MM:SS
  * note the *T* in the example with time
  * as literals in code, they should be surrounded with angle brackets (`<>`)
  * when inputting into a prompt the angle brackets should be omitted
