# Lambda Cube

Lambda Cube is a personal project aimed at implementing every corner of the lambda cube, exploring different variations of lambda calculus and serving as a foundation for experimenting with functional programming and type theory.

## Goals

The primary goals of the project are as follows:

1. Implement an extendable core calculus that allows for the addition of various extensions.
2. Develop three main features: System F, dependent types and type constructors, and combine them into different systems like simply typed lambda calculus and calculus of construction.

## Features

The project includes the following key features:

1. **Core Calculus**: An extendable codebase that serves as the foundation for the project and supports untyped lambda calculus.

2. **Simply Typed Calculus**: Elaboration of the core calculus to support simply typed lambda calculus.

3. **Advanced Features**:
   - **System F**: Implementation of System F, a polymorphic lambda calculus that introduces parametric polymorphism.
   - **Dependent Types**: Support for dependent types.
   - **Type Constructors**:  Support for type constructors, enabling more expressive type systems.

## Getting Started

As the project is currently in development, there are no completed features available for use. However, if you're interested in exploring or contributing to the project, you can follow these steps:

1. Clone the repository to your local machine:
   ```
   git clone <repository-url>
   ```

2. Build the project using Cargo, the Rust package manager:
   ```
   cargo build
   ```

## Future Development

While the project is still evolving, potential future developments include:

- Providing a Virtual Machine for the project to enable compilation and optimization, aiming for improved performance.
- Continuously refining and expanding the implementation of different lambda calculus variations.
- Welcoming contributions and feedback from the community to enhance the project.

## Terminology

To help you navigate the project, here are some terms you may encounter:

- [**Lambda Calculus**](https://en.wikipedia.org/wiki/Lambda_calculus): A formal system in mathematical logic and computer science used to represent and manipulate functions.
- [**System F**](https://en.wikipedia.org/wiki/System_F): A polymorphic lambda calculus that introduces parametric polymorphism, allowing functions to be generic over types.
- [**Dependent Types**](https://en.wikipedia.org/wiki/Dependent_type): Types that depend on values, enabling more precise and expressive type systems.
- [**Type Constructors**](https://en.wikipedia.org/wiki/Type_constructor): Functions that take types as input and produce new types as output, allowing the construction of complex types.

## Contributing

Although the project is in its early stages, feedback and suggestions are welcome. If you encounter any issues or have any advice, please feel free to open an issue on the repository.

## License

This project is licensed under the [MIT License](LICENSE).
