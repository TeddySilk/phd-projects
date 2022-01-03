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