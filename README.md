# Neural Network Example

[![Build Status](https://github.com/enzoh/neural-network-example/workflows/build/badge.svg)](https://github.com/enzoh/neural-network-example/actions?query=workflow%3Abuild)

A simple feedforward neural network.

## Overview

This package provides a simple feedforward neural network that can recognize handwritten digits from the MNIST database. The underlying model consists of an input layer with 784 nodes, a hidden layer with 32 nodes, and an output layer with 10 nodes. The input layer corresponds to grayscale pixels, and the output layer to labels. The relationship between the input layer and hidden layer is described by a weight matrix W₁ and bias vector b₁, and the relationship between the hidden layer and output layer is described by a weight matrix W₂ and bias vector b₂. The method of gradient descent is used to calculate the weight matrices and bias vectors by minimizing a loss function defined by F(b₁, b₂, W₁, W₂) = ½ * (Softmax(b₂ + W₂ * ReLU(b₁ + W₁ * x)) - y)². 

## Prerequisites

- [Cabal](https://www.haskell.org/cabal)

## Model Training

Execute the following command to train the model.

```
cabal run train
```

This will create a file called `model.cbor` in the current working directory.

## Model Testing

Execute the following command to test the model.

```
cabal run test
```

This will print the accuracy of the model.

To see which ones the model got wrong, run with `--ghc-options="-DTRACE"`.