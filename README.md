# Projects related to my PHD studies
This repository acts as the hub for all (private) coding projects born during my PhD studies. The projects are primarily related to courses or data treatment and visualization.

Currently, it contains results for:
  1. (January 2022) Course 41391: High-Performance Computing: Fortran, OpenMP, and MPI

# 1. High-Performance Computing: Fortran, OpenMP, and MPI
This folder contains the results to the exercises in the PHD course "High-Performance Computing: Fortran, OpenMP, and MPI" taken at DTU. I have written two simple bash-scripts to alleviate the compiling and running of the scripts, and to automate frequent field-plots using gnuplot-x11.

To use these, ensure that the either i) that $PATH points to the folder "./phd-projects/41391_hpc_fortran_openmp_mpi/", or ii) generate an alias for easy access. For example,

``` shell
# gnuplot and fortran compiler shortcuts
alias gp3='~/git/phd-projects/41391_hpc_fortran_openmp_mpi/gnuplot3d.sh'
alias fcomp='~/git/phd-projects/41391_hpc_fortran_openmp_mpi/fcompile.sh'
```

These two scripts are simple bash scripts, which take either a command input or a terminal input:

``` shell
# fcompile.sh
# ---------------------------------------------- #
# A simple bash-script to automate compiling and #
# running a fortran script                       #
# ---------------------------------------------- #

# check if command line is empty
if [ -z "$1"]
then
    echo "Input Fortran script name (without .f95):"
    read script_name
else
    script_name=$1
fi

# compile and run script
f95 -free "${script_name}.f95" -o "${script_name}.out"
${script_name}.out
```

``` shell
# gnuplot3d.sh
# ----------------------------------------------------- #
# A simple bash-script to automate the gnuplot plotting #
# ----------------------------------------------------- #

# check if command line is empty
if [ -z "$1"]
then
    echo "Input field data name (without .dat):"
    read field_data
else
    field_data=$1
fi

# plot field data
gnuplot-x11 -e "sp'${field_data}.dat' u 1:2:3 w l; set term post eps color solid; set out '${field_data}.eps'; repl"

# open output in okular
okular "${field_data}.eps"
```

