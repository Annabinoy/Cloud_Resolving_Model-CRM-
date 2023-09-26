## Overview
At this stage of the project we have implemented advection in 3d space. Fortran was used to generate the code and python was used for plotting. The code for the project is stored in a private repository on github(link). The data generated is stored in the bin folder along with the executable formed after compiling the fortran code present in the fortran folder in src.

## Formulas
The advection equation in the case where we assume incompressible flow was used to implement the model


FTCS(Forward in time and central in space) scheme was used and hence the above equation is discretized to:



				

## Code layout
Code mainly consists of four modules and a main program. All the fortran code is saved in the fortran folder present inside the src folder in github. The main program(Adv.f95) reads all the inputs/parameters from a text file named advparam.txt present in the bin folder and passes it to various functions in the module. The module names and their role is mentioned in the table below.

### mod_grid.f95
contains the class Grid which has variables to store the various parameters of the grid we are considering. It also contains functions which convert the grid from x,y coordinates to lat,long coordinates.
### mod_diff.f95
contains the functions for implementing the various schemes for solving differential equations. Now contains two finite difference approximations:forward difference and centered difference.
### mod_misc.f95
contains miscellaneous functions like file_retrieve and file_write which are required for reading and writing data as well as other parameters.
### mod_initial
contains all the functions and subroutines to initialize array with an initial function, right now it contains functions for storing a gaussian in the case 1d, 2d and 3d matrices respectively


*CREDITS:  **Anna Binoy** ,  **Oommen P. Jose** (both of them are 4th year Int. M.Sc.(2019-2024) Students of National Institute of Science Education and Research(NISER)) under the guidance of  **Dr. Jaya Khanna**  (Assistant Professor,SEPS,NISER) *


