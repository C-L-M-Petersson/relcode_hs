RUN=$(grep -e "^run\b" kraken.cfg | awk '{print $3}')
FWHM=$(grep -e "^fwhm\b" kraken.cfg | awk '{print $3}')
OMEGA0=$(grep -e "^omega0\b" kraken.cfg | awk '{print $3}')
EFINALINDEX=$(grep -e "^eFinalIndex\b" kraken.cfg | awk '{print $3}')

EMIN=11.5
EMAX=13.2

OUTFILEDIRBASE=outFiles/KRAKEN/run$RUN/
OUTIMAGEDIRBASE=outImage/run$RUN/

run1ph()
{
    echo "Treating 1ph: $1 -> $2"
    ./Relcode.x --runKRAKEN1ph True  --runKRAKEN2ph False \
                --coherent1ph  None                       \
                --kappas0 [$1] --kappas1 [$2]

    OUTFILEDIR="$OUTFILEDIRBASE"rho1ph.OMEGA0=$OMEGA0.FWHM=$FWHM/
    OUTIMAGEDIR="$OUTIMAGEDIRBASE"rho1ph.OMEGA0=$OMEGA0.FWHM=$FWHM/

    OUTFILERHO="$OUTFILEDIR"rho.k0=[$1].k1=[$2].res
    OUTFILEPSI="$OUTFILEDIR"Psi.k0=[$1].k1=[$2].res
    OUTIMAGE="$OUTIMAGEDIR"rho.k0=[$1].k1=[$2].png

    mkdir -p $OUTIMAGEDIR

    gnuplot -e "inFileRho='$OUTFILERHO'" \
            -e "inFilePsi='$OUTFILEPSI'" \
            -e "outFile='$OUTIMAGE'"     \
            -e "emin='$EMIN'"            \
            -e "emax='$EMAX'"            \
            -e "ebordershift='0'"        \
            -e "fromKappas='$1'"         \
            -e "toKappas='$2'" gpFiles/plot1ph.gp
}

run2ph()
{
    echo "Treating 2ph: $1 -> $2 -> $3"
    ./Relcode.x --runKRAKEN1ph False --runKRAKEN2ph True    \
                --coherent1ph  All   --coherent2ph  None    \
                --kappas0 [$1] --kappas1 [$2] --kappas2 [$3]

    OUTFILEDIR="$OUTFILEDIRBASE"rho2ph.OMEGA0=$OMEGA0.FWHM=$FWHM.eFinalIndex=$EFINALINDEX/
    OUTIMAGEDIR="$OUTIMAGEDIRBASE"rho2ph.OMEGA0=$OMEGA0.FWHM=$FWHM.eFinalIndex=$EFINALINDEX/

    OUTFILERHO="$OUTFILEDIR"rho.k0=[$1].k1=[$2].k2=[$3].res
    OUTFILEPSI="$OUTFILEDIR"Psi.k0=[$1].k1=[$2].k2=[$3].res
    OUTIMAGE="$OUTIMAGEDIR"rho.k0=[$1].k1=[$2].k2=[$3].png

    mkdir -p $OUTIMAGEDIR

     gnuplot -e "inFileRho='$OUTFILERHO'" \
             -e "inFilePsi='$OUTFILEPSI'" \
             -e "outFile='$OUTIMAGE'"     \
             -e "emin='$EMIN'"            \
             -e "emax='$EMAX'"            \
             -e "ebordershift='0'"        \
             -e "fromKappas='$1'"         \
             -e "byKappas='$2'"           \
             -e "toKappas='$3'" gpFiles/plot2ph.gp
}

run1ph 1    -1
run1ph 1    2
run1ph 1    -1,2

run1ph -2   -1
run1ph -2   2
run1ph -2   -3
run1ph -2   -1,2
run1ph -2   -1,-3
run1ph -2   2,-3
run1ph -2   -1,2,-3

run1ph 1,-2 -1
run1ph 1,-2 2
run1ph 1,-2 -3
run1ph 1,-2 -1,2
run1ph 1,-2 -1,-3
run1ph 1,-2 2,-3
run1ph 1,-2 -1,2,-3



run2ph 1  -1        1
run2ph 1  -1        -2
run2ph 1  -1        1,-2

run2ph 1  2         1
run2ph 1  2         -2
run2ph 1  2         3
run2ph 1  2         1,-2
run2ph 1  2         1,3
run2ph 1  2         -2,3
run2ph 1  2         1,-2,3

run2ph 1  -1,2      1
run2ph 1  -1,2      -2
#run2ph 1  -1,2      3
run2ph 1  -1,2      1,-2
run2ph 1  -1,2      1,3
run2ph 1  -1,2      -2,3
run2ph 1  -1,2      1,-2,3

run2ph -2 -1        1
run2ph -2 -1        -2
run2ph -2 -1        1,-2

run2ph -2 2         1
run2ph -2 2         -2
run2ph -2 2         3
run2ph -2 2         1,-2
run2ph -2 2         1,3
run2ph -2 2         -2,3
run2ph -2 2         1,-2,3

run2ph -2 -3        -2
run2ph -2 -3        3
run2ph -2 -3        -4
run2ph -2 -3        -2,3
run2ph -2 -3        -2,-4
run2ph -2 -3        3,-4
run2ph -2 -3        -2,3,-4

run2ph -2 -1,2      1
run2ph -2 -1,2      -2
#run2ph -2 -1,2      3
run2ph -2 -1,2      1,-2
run2ph -2 -1,2      1,3
run2ph -2 -1,2      -2,3
run2ph -2 -1,2      1,-2,3

#run2ph -2 -1,-3     1
run2ph -2 -1,-3     -2
#run2ph -2 -1,-3     3
#run2ph -2 -1,-3     -4
run2ph -2 -1,-3     1,-2
run2ph -2 -1,-3     1,-3
run2ph -2 -1,-3     1,4
run2ph -2 -1,-3     -2,3
run2ph -2 -1,-3     -2,-4
run2ph -2 -1,-3     3,-4
run2ph -2 -1,-3     1,-2,3
run2ph -2 -1,-3     1,-2,-4
run2ph -2 -1,-3     1,3,-4
run2ph -2 -1,-3     -2,3,-4
run2ph -2 -1,-3     1,-2,3,-4

#run2ph -2 2,-3      1
run2ph -2 2,-3      -2
run2ph -2 2,-3      3
#run2ph -2 2,-3      -4
run2ph -2 2,-3      1,-2
run2ph -2 2,-3      1,3
run2ph -2 2,-3      1,-4
run2ph -2 2,-3      -2,3
run2ph -2 2,-3      -2,-4
run2ph -2 2,-3      3,-4
run2ph -2 2,-3      1,-2,3
run2ph -2 2,-3      1,-2,-4
run2ph -2 2,-3      1,3,-4
run2ph -2 2,-3      -2,3,-4
run2ph -2 2,-3      1,-2,3,-4

run2ph -2 -1,2,-3   1
run2ph -2 -1,2,-3   -2
#run2ph -2 -1,2,-3   3
#run2ph -2 -1,2,-3   -4
run2ph -2 -1,2,-3   1,-2
run2ph -2 -1,2,-3   1,3
run2ph -2 -1,2,-3   1,-4
run2ph -2 -1,2,-3   -2,3
run2ph -2 -1,2,-3   -2,-4
#run2ph -2 -1,2,-3   3,-4
run2ph -2 -1,2,-3   1,-2,3
run2ph -2 -1,2,-3   1,-2,-4
run2ph -2 -1,2,-3   1,3,-4
run2ph -2 -1,2,-3   -2,3,-4
run2ph -2 -1,2,-3   1,-2,3,-4

run2ph 1,-2 -1      1
run2ph 1,-2 -1      -2
run2ph 1,-2 -1      1,-2

run2ph 1,-2 2       1
run2ph 1,-2 2       -2
run2ph 1,-2 2       3
run2ph 1,-2 2       1,-2
run2ph 1,-2 2       1,3
run2ph 1,-2 2       -2,3
run2ph 1,-2 2       1,-2,3

#run2ph 1,-2 -3

run2ph 1,-2 -1,2    1
run2ph 1,-2 -1,2    -2
#run2ph 1,-2 -1,2    3
run2ph 1,-2 -1,2    1,-2
run2ph 1,-2 -1,2    1,3
run2ph 1,-2 -1,2    -2,3
run2ph 1,-2 -1,2    1,-2,3

#run2ph 1,-2 -1,-3   1
run2ph 1,-2 -1,-3   -2
#run2ph 1,-2 -1,-3   3
#run2ph 1,-2 -1,-3   -4
run2ph 1,-2 -1,-3   1,-2
run2ph 1,-2 -1,-3   1,3
run2ph 1,-2 -1,-3   1,-4
run2ph 1,-2 -1,-3   -2,3
run2ph 1,-2 -1,-3   -2,-4
run2ph 1,-2 -1,-3   3,-4
run2ph 1,-2 -1,-3   1,-2,3
run2ph 1,-2 -1,-3   1,-2,-4
run2ph 1,-2 -1,-3   1,3,-4
run2ph 1,-2 -1,-3   -2,3,-4
run2ph 1,-2 -1,-3   1,-2,3,-4

run2ph 1,-2 2,-3    1
run2ph 1,-2 2,-3    -2
run2ph 1,-2 2,-3    3
run2ph 1,-2 2,-3    -4
run2ph 1,-2 2,-3    1,-2
run2ph 1,-2 2,-3    1,3
run2ph 1,-2 2,-3    1,-4
run2ph 1,-2 2,-3    -2,3
run2ph 1,-2 2,-3    -2,-4
run2ph 1,-2 2,-3    3,-4
run2ph 1,-2 2,-3    1,-2,3
run2ph 1,-2 2,-3    1,-2,-4
run2ph 1,-2 2,-3    1,3,-4
run2ph 1,-2 2,-3    -2,3,-4
run2ph 1,-2 2,-3    1,-2,3,-4

#run2ph 1,-2 -1,2,-3 1
run2ph 1,-2 -1,2,-3 -2
#run2ph 1,-2 -1,2,-3 3
#run2ph 1,-2 -1,2,-3 -4
run2ph 1,-2 -1,2,-3 1,-2
run2ph 1,-2 -1,2,-3 1,3
run2ph 1,-2 -1,2,-3 1,-4
run2ph 1,-2 -1,2,-3 -2,3
run2ph 1,-2 -1,2,-3 -2,-4
#run2ph 1,-2 -1,2,-3 3,-4
run2ph 1,-2 -1,2,-3 1,-2,3
run2ph 1,-2 -1,2,-3 1,-2,-4
run2ph 1,-2 -1,2,-3 1,3,-4
run2ph 1,-2 -1,2,-3 -2,3,-4
run2ph 1,-2 -1,2,-3 1,-2,3,-4
