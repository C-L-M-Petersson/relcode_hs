#!/bin/sh

link()
{
    [ -e $2 ] || ln -s $1 $2
}

for RUNDIR in outImage/*
do

for TREATMENT in $(ls -d $RUNDIR/rho1ph* | cut -d. -f2- | sort -u)
do

mkdir -p $RUNDIR/$TREATMENT

[ -e $RUNDIR/rho1ph.$TREATMENT/ ] && \
for ONEPHOTONKAPPAS in $(ls $RUNDIR/rho1ph.$TREATMENT/ | cut -d. -f2- | rev \
                                                       | cut -d. -f2- | rev )
do


mkdir -p "$RUNDIR/$TREATMENT/"
link "../rho1ph.$TREATMENT/rho.$ONEPHOTONKAPPAS.png" \
     "$RUNDIR/$TREATMENT/rho1ph.$ONEPHOTONKAPPAS.png"

for IR in $(ls -d $RUNDIR/rho2ph.$TREATMENT* | rev | cut -d. -f1 | rev )
do

mkdir -p "$RUNDIR/$TREATMENT/$IR.$ONEPHOTONKAPPAS"
link "../../rho1ph.$TREATMENT/rho.$ONEPHOTONKAPPAS.png" \
     "$RUNDIR/$TREATMENT/$IR.$ONEPHOTONKAPPAS/rho1ph.$ONEPHOTONKAPPAS.png"

for TWOPHOTONKAPPA in $(ls $(echo "$RUNDIR/rho2ph.$TREATMENT.$IR/rho.$ONEPHOTONKAPPAS.*" | sed -e 's/\[/\\[/g' -e 's/\]/\\]/g') | rev | cut -d. -f2 | rev )
do

link "../../rho2ph.$TREATMENT.$IR/rho.$ONEPHOTONKAPPAS.$TWOPHOTONKAPPA.png" \
     "$RUNDIR/$TREATMENT/$IR.$ONEPHOTONKAPPAS/rho2ph.$ONEPHOTONKAPPAS.$TWOPHOTONKAPPA.png"

done
done
done
done
done
