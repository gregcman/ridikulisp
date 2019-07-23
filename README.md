## A Toy Language Based On A Really Simple Interpreter

```
;;This is the entire interpreter!
(defun step (k) (loop (setf (caadar k) (cdaaar k) k (cadr k))))
```

The program is completely made out of cons cells.

## Features

Turing-Completeness

Benefits: 

Since the data structure is so simple, adding new features should be trivial. Like networking, live reloading, security and capabilities.

Drawbacks:

The simplicity of the interpreter means that the code is large and essentially unreadable after it is compiled.

Implemented:

- Garbage collection

Not Implemented: 

- Basically everything else. 

```
;;All these forms can be implemented by connecting cons cells together.
eq
if
atom
car
cdr
```

## Background

I was inspired by the "One Instruction Set Computers" found here: https://esolangs.org/wiki/OISC
