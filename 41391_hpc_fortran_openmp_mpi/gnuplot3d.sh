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