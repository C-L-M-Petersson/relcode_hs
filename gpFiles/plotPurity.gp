#!/bin/gnuplot

reset

set terminal pngcairo size 2400,1200 font 'Verdana,24'
set output outFile

lmarga=0.07
rmarga=0.42
lmargb=0.55
rmargb=0.90
tmargina=0.33
bmargina=0.93
tmarginb=0.10
bmarginb=0.30

set xlabel "FWHM (eV)"
set ylabel "Purity"
set title ("Purity: κ_{g} = ".fromKappas." κ_{i} = ".byKappas.", κ_{f} = ".toKappas) offset 0,0.5

plot[0.1:0.7][0.5:1] inFilePurity using 1:2 w l lw 4 title "1ph" \
                   , inFilePurity using 1:4 w l lw 4 title "2ph"
