# About

This project is a Rust implementation of both C bytecode virtual machine and Java tree-walk interpreters for Lox object-oriented programming language, featured in the book "Crafting Interpreters".

Unsafe code is forbidden in both crates.

# How to launch the bytecode virtual machine interpreter

Make sure [Rust](https://www.rust-lang.org/tools/install) is installed.

Then you can launch the following Lox code, which computes the 70th Fibonacci number :
~~~
cd lox_bytecode_virtual_machine
cargo run --release test_lox_scripts/optimized_fibonacci.lox
~~~

# How to launch the tree-walk interpreter

Make sure [Rust](https://www.rust-lang.org/tools/install) is installed.

Then you can launch the following Lox code, which computes the 70th Fibonacci number :
~~~
cd lox_tree_walk/
cargo run --release test_lox_scripts/optimized_fibonacci.lox
~~~

# How to launch unit tests in both cases

~~~
cargo test
~~~