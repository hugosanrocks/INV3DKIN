#
# libraries
#

# Library variables
LTOOLS_BOX = ../../TOOLS_BOX

LOPTIM = $(LTOOLS_BOX)/OPTIMIZATION/lib -lPSTD
IOPTIM  = $(LTOOLS_BOX)/OPTIMIZATION/COMMON/include

#
# compiler options
#
F77_OPTION=-O3
F90_OPTION=-O3
CC_OPTION= -O3

#
# compilers
#
F77=gfortran
F90=gfortran
CC=gcc

# libs
LIBS = -L $(LOPTIM)
