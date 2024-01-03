# Multitape Turing Emulator

## Compilation and usage

You can use CMake to build the program:

1. `cmake -B build`
2. `cd ./build; make`
3. The `turing` executable will be in the `bin` folder.

Or you can directly compile the program using gcc:

```sh
$ gcc turing.c -o turing
$ ./turing
usage: turing [-v|--verbose] [-h|--help] <tm> <input>
```

## Example turing program

An example turing program is provided. A detailed description of the syntax is ommited.

## License

`turing.c` is licensed under **GPL-3.0-or-later**.

**Disclaimer: DO NOT COPY THIS PROJECT FOR ANY COURSE PROJECT AT SCHOOL.**
