# Hython

## Introduction

Hython is an imperative language that resembles a high-level language like Python, which emphasizes code readability with the use of significant indentation. With existing properties of Python, such as list comprehensions, a statically scoped implementation, strict evaluation, anonymous functions, and higher-order functions, Hython can be very easy to learn and use. On the other hand, Hython is specifically designed for high-dimensional matrix calculations. Hence, it also adopts features like a strongly and statically typed system, and modules that contain common matrix manipulations. These additional features that do not exist in Python make matrix manipulation extremely fast and easy. Hython will be an appropriate introduction for users without prior programming experience who want to effectively perform complex matrix calculations. 

## Work done so far:
We have finished most of the work in the lexical and syntax analysis phase (ie scanner.mll and parser.mly). The following is a list of items we currently have implemented in the frontend:

### Parser + Scanner
* Punctuation
* Mathematical operators
* Primitive type declarations
* Tensor type declarations
* Custom functions
* Associativity

#### Parser rules
* Primitive program structure
* Types
* Variable declaration
* Import
* Function declaration
* Function formals
* Statements
* Expressions
  * Literals
  * Plus
  * Minus
  * Multiply
  * Divide
* Value assignment
* Tensor

### Ast
* Expression
* Statement 
* Function

## Work left until completion
The following are the items we will implement
| Scanner + Parser                         | Assigned To |
| ---------------------------------------- | ----------- |
| Refine program rules                     | Dawei       |
| Loops (while, for)                       | Dawei       |
| Relational operators                     | Luna        |
| Logical operators                        | Luna        |
| Expressions (mod, matrix multiplication) | Luna        |
| Function call                            | Xiaoning    |
| Lambda function definition               | Xiaoning    |
| Conditional operators                    | Philip      |
| Tensor operation                         | Philip      |
| Errors and expressions                   | Yan         |
| Types (Void, String)                     | Yan         |
| Global variables                         | Yan         |

| Semantic Checker    | Assigned To |
| ------------------- | ----------- |
| Variable assignment |             |
| Tensor              | Philip      |
| Function            | Xiaoning    |

Items to implement for future steps will be added.

## Timeline
The following is a timeline of items we will implement and their estimated completion date
| Item                         | Estimated Completion Date |
| ---------------------------- | ------------------------- |
| Scanner + Parser             | 4/9                       |
| Semantic checker             | 4/16                      |
| Sast                         | 4/16                      |
| Intermediate code generation | 4/23                      |
| Optimization                 | 4/23                      |
| Code generation              | 4/30                      |
| Additional features          | 5/7                       |
