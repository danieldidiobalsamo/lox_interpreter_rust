# About

This project is a Rust implementation of the Java tree-walk interpreter for Lox object-oriented programming language, featured in the book "Crafting Interpreters".

Unsafe code is forbidden in the whole crate.

# How to launch

Make sure [Rust](https://www.rust-lang.org/tools/install) is installed.

Then you can launch the following Lox code, which computes the 70th Fibonacci number :
~~~
cargo run --release test_lox_scripts/optimized_fibonacci.lox
~~~

# How to launch unit tests

~~~
cargo test
~~~