# Projects related to my PHD studies
This repository acts as the hub for all (private) coding projects born during my PhD studies. The projects are primarily related to courses or data treatment and visualization.

Currently, it contains results for:
  1. (January 2022) Course 41391: High-Performance Computing: Fortran, OpenMP, and MPI

# 1. High-Performance Computing: Fortran, OpenMP, and MPI
This folder contains the results to the exercises in the PHD course "High-Performance Computing: Fortran, OpenMP, and MPI" taken at DTU. I have a simple bash-scripts to automate frequent field-plots using gnuplot-x11. It is bare-bones, just to quickly check if it looks physical.

To use this, ensure that the either i) that $PATH points to the folder "./phd-projects/41391_hpc_fortran_openmp_mpi/", or ii) generate an alias for easy access. For example,

``` shell
# gnuplot and fortran compiler shortcuts
alias gp3='~/git/phd-projects/41391_hpc_fortran_openmp_mpi/gnuplot3d.sh'
```

The script is as follows:

``` shell
#!/bin/bash
# ----------------------------------------------------- #
# A simple bash-script to automate the gnuplot plotting #
# ----------------------------------------------------- #

# check if command line is empty
if [[ -z "$1" ]]
then
    echo -n "Input field data name >> "
    read field_data
else
    field_data=$1
fi

# check if input contains file extension 
IFS='.'                             # dot (.) is set as delimiter
read -ra INPUT <<< ${field_data}   # str is read into an array as tokens separated by IFS
IFS=' '

# if input does not contain a file extension, assume it is .dat
if [[ ${#INPUT[@]} == 1 ]]
then
    gnuplot-x11 -e "sp'${INPUT[0]}.dat' u 1:2:3 w l; set term post eps color solid; set out '${INPUT[0]}.eps'; repl"
    okular "${INPUT[0]}.eps"
elif [[ ${#INPUT[@]} == 2 ]]
then
    gnuplot-x11 -e "sp'${INPUT[0]}.${INPUT[1]}' u 1:2:3 w l; set term post eps color solid; set out '${INPUT[0]}.eps'; repl"
    okular "${INPUT[0]}.eps"
else
    echo "Bad input. Too many file extensions."
fi
```

