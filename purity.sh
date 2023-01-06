#!/bin/sh

OUTFILEDIR=outFiles/KRAKEN/run4/PURITY/
OUTIMAGEDIR=outImage/KRAKEN/run4/PURITY/
OUTFILEPURITY="$OUTFILEDIR"all.res
OUTIMAGEPURITY="$OUTIMAGEDIR"all.png

echo -n "" > "$OUTFILEDIR"all.res

for FWHM in 0.100 0.120 0.140 0.160 0.180 \
            0.200 0.220 0.240 0.260 0.280 \
            0.300 0.320 0.340 0.360 0.380 \
            0.400 0.420 0.440 0.460 0.480 \
            0.500 0.520 0.540 0.560 0.580 \
            0.600 0.620 0.640 0.660 0.680 \
            0.700
do
    OUTFILEBASE=\${outFileBase}/run\${run}/PURITY/
    PURITY1PHFILE=$OUTFILEBASE/purity1ph.FWHM=$FWHM.res
    PURITY2PHFILE=$OUTFILEBASE/purity2ph.FWHM=$FWHM.res
    CONCURRENCE1PHFILE=$OUTFILEBASE/concurrence1ph.FWHM=$FWHM.res
    CONCURRENCE2PHFILE=$OUTFILEBASE/concurrence2ph.FWHM=$FWHM.res
    echo "Treating FWHM=$FWHM"
    ./run.x --fwhm                  $FWHM               \
            --runKRAKEN1ph          True                \
            --runKRAKEN2ph          False               \
            --saveRho1ph            False               \
            --savePsi1ph            False               \
            --savePurity1ph         True                \
            --saveConcurrence1ph    True                \
            --outFilePurity1ph      $PURITY1PHFILE      \
            --outFilePurity2ph      $PURITY2PHFILE      \
            --outFileConcurrence1ph $CONCURRENCE1PHFILE \
            --outFileConcurrence2ph $CONCURRENCE2PHFILE \
            --coherent1ph           Spin
    ./run.x --fwhm                  $FWHM               \
            --runKRAKEN1ph          False               \
            --runKRAKEN2ph          True                \
            --saveRho2ph            False               \
            --savePsi2ph            False               \
            --savePurity2ph         True                \
            --saveConcurrence2ph    True                \
            --outFilePurity1ph      $PURITY1PHFILE      \
            --outFilePurity2ph      $PURITY2PHFILE      \
            --outFileConcurrence1ph $CONCURRENCE1PHFILE \
            --outFileConcurrence2ph $CONCURRENCE2PHFILE \
            --coherent1ph           All                 \
            --coherent2ph           Spin

    echo -n "$FWHM " >> "$OUTFILEDIR"all.res
    paste "$OUTFILEDIR"purity1ph.FWHM=$FWHM.res      \
          "$OUTFILEDIR"concurrence1ph.FWHM=$FWHM.res \
          "$OUTFILEDIR"purity2ph.FWHM=$FWHM.res      \
          "$OUTFILEDIR"concurrence2ph.FWHM=$FWHM.res \
            >> "$OUTFILEPURITY"
done

mkdir -p $OUTIMAGEDIR
gnuplot -e "inFilePurity='$OUTFILEPURITY'" \
        -e "outFile='$OUTIMAGEPURITY'"     \
        -e "fromKappas='1,-2'"             \
        -e "byKappas='-1,2,-3'"            \
        -e "toKappas='1,-2,3,-4'" gpFiles/plotPurity.gp
