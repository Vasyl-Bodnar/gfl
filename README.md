# gfl

GFL is a Graph Functional (interpreted) Language written in Zig. 

It is designed around defining and operating on graphs with turing completeness from lambda functions and various primitives.

## Compilation

To compile GFL, you need to have Zig installed on your system. 
There are no dependencies other than Zig standard library.
Then you can simply run `zig build` in the root directory of the cloned project.

The binary will be generated in the `zig-out/bin` directory. 
This binary is a simple REPL and file interpreter for the GFL language. 

The library will be available in the `zig-out/lib` directory.
Note that the library is not yet fully implemented and makes some assumptions about the input and use.

## Syntax

GFL syntax is not completely defined yet, and it may appear to be a bit weird. 

The idea was a rather functional and simple to parse syntax.

Here is how it looks like right now:
```
number: 1 
string: "hello"
imm_graph: {1 2 3} # 1 -> 2 -> 3
weighted_graph: @{1 2 3} # 2 -> 3 with weight 1
exp_dir_graph: *|1 2 3> # 1 -> 2 -> 3 with possibility of adding more vertices
lambda: \x -> x + 1

# Some functions
print(graph)
print(add(1 2 3 4))
print(pretty(\x -> x))
print(lambda(1))
```

## Implementation

The implementation is still in progress. 
Not all of prelude functions are implemented. 
The language is not yet fully tested.
There are cases where the language will panic or not work as expected.
