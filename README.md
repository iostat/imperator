# imperator
See the examples folder to get a sense of how it works.

## Building / Running
You'll just need the Haskell `stack`
```
git clone https://github.com/iostat/imperator.git
cd imperator
stack setup
stack install
stack exec imperator examples/some_file.dsl
```

## Quirks
1. last statement in a file shouldn't have a semicolon after it. ditto for inside parens as part of a conditional branch. 
2. if you miss semicolons separating your statements, they probably wont run with no parser error given
3. DateTimes are ISO 8601
  * i.e. YYYY-MM-DD or YYYY-MM-DD*T*HH:MM:SS
  * note the *T* in the example with time
  * as literals in code, they should be surrounded with angle brackets (`<>`)
  * when inputting into a prompt the angle brackets should be omitted
