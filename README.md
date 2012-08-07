# Fortran 90/UMFPACK Sparse Matrix Solver Example

This program demonstrates how to solve a sparse, square matrix using the [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/) library in a Fortran 90 program.

This example is intended as a starting point for people who want to use [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/) with Fortran 90. It covers the basics of creating a sparse format matrix, solving the equation Ax = b, and compiling/linking against [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/)

## Quickstart

### Dependancies 

To get started, you will need to install [UMFPACK](http://www.cise.ufl.edu/research/sparse/umfpack/). On Ubuntu based systems these are provided by the following packages

    apt-get install suitesparse libsuitesparse-dev

### Compile

    gfortran umf4_f77wrapper.c sparse.F90 -lumfpack -o sparse

### Run

    $ ./sparse 
    1.45519152283668518E-011
    1.45519152283668518E-011
