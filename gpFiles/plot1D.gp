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

set pm3d map #interpolate 20,20
unset key
set multiplot layout 2,2 title ("κ_{g} = ".fromKappas.", κ_{i} = ".toKappas) offset 0,0.5

unset xtics

set palette defined ( 0 '#000000',\
                      1 '#000fff',\
                      2 '#0090ff',\
                      3 '#0fffee',\
                      4 '#90ff70',\
                      5 '#ffee00',\
                      6 '#ff7000',\
                      7 '#ee0000',\
                      8 '#7f0000')
set cbrange [0:]
set title "amp(ρ)" offset 0,-0.7
set ylabel "ϵ_2"
set lmargin at screen lmarga
set rmargin at screen rmarga
set tmargin at screen tmargina
set bmargin at screen bmargina
splot[emin+ebordershift:emax-ebordershift][emin+ebordershift:emax-ebordershift] inFileRho using 1:2:3

set palette defined ( 0 "blue", 0.5 "white", 1 "red" )
set cbrange [*:*]
set title "phase(ρ)"
unset ytics
unset ylabel
set cbtics
set lmargin at screen lmargb
set rmargin at screen rmargb
splot[emin+ebordershift:emax-ebordershift][emin+ebordershift:emax-ebordershift] inFileRho using 1:2:4

unset pm3d
set xlabel "ϵ_1"
set xtics
unset ytics

set ylabel "amp(Ψ)"
unset title
#plot inFilePsi u 1:2
set lmargin at screen lmarga
set rmargin at screen rmarga
set tmargin at screen tmarginb
set bmargin at screen bmarginb
plot[emin+ebordershift:emax-ebordershift] inFilePsi u 1:2 w l

set ylabel "phase(Ψ)"
set ytics #("-π" -3.14159, 0 0, "π" 3.14159)
set lmargin at screen lmargb
set rmargin at screen rmargb
plot[emin+ebordershift:emax-ebordershift][:] inFilePsi u 1:3 w l
#plot[emin+ebordershift:emax-ebordershift][-pi:pi] inFilePsi u 1:3

unset multiplot
unset output

