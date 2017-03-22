binsect (Forked from [insect](https://github.com/sharkdp/insect) by @sharkdp)
=======

A fast, repl-style binary calculator for the web and for the terminal.

[![insect](media/insect-32x32.png)](https://shark.fish/insect/)

* [**Web version**](https://soupi.github.io/insect)
* [Terminal version](#install)

Features
--------
  Supported functions: `add`, `sub`, `mul`, `and`, `or`, `xor`,
  `shr`, `shl`, `complement`.

- Explicit representation conversions
  ```
  \xf -> decimal
  \b101 -> hex
  32 -> binary
  ```

- Variable assigments:
  Predefined constants: maximum and minimum 32bit int value (`maxInt`), (`minInt`), ...

  You can use `ans` (answer) to refer the the result of the last calculation.

- Commands:
  ```
  help, ?
  list, ls
  reset
  ```

Install
-------
In addition to the web interface, there is also a command line version which can by installed via [npm](https://www.npmjs.com/package/insect):
```sh
npm install -g insect
```

Build
-----
```sh
bower install
npm install
pulp -w browserify --skip-entry-point -m Insect --standalone Insect -O -t insect.js
```
