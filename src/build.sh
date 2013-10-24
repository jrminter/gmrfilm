#!/bin/bash

gfortran -c edge.for

gfortran -c absionrt.for

gfortran -c omega.for

gfortran -c ei.for

gfortran -c erfc.for

gfortran -c eidp.for

gfortran -c effyld.for

gfortran -c mu.for

gfortran -c trnsprob.for

gfortran -c xray.for

gfortran -c znl.for

gfortran -c layrelem.for

gfortran -c qe0.for

gfortran -c rjump.for

gfortran -c stoich0.for

gfortran -c stoich1.for

gfortran -c abscoeff.for

gfortran -c arfc.for

gfortran -c paplimts.for

gfortran -c papei.for

gfortran -c papex.for

gfortran -c papexei.for

gfortran -c ppint1.for

gfortran -c ppint2.for

gfortran -c mac.for

gfortran -c rb2a.for

gfortran -c bastbeta.for

gfortran -c macstd.for

gfortran -c charfdta.for

gfortran -c papint.for

gfortran -c papwt.for

gfortran -c pap.for

gfortran -c scale.for

gfortran -c phirzeq.for

gfortran -c papfluor.for

gfortran -c tripint.for

gfortran -c charflr.for

gfortran -c chiov.for

gfortran -c ck.for

gfortran -c lookup.for

gfortran -c wtfract.for

gfortran -c compstd.for

gfortran -c contflr.for

gfortran -c differen.for

gfortran -c dlines.for

gfortran -c helpfile.for

gfortran -c elements.for

gfortran -c fixlayer.for

gfortran -c fluorprn.for

gfortran -c ftest.for

gfortran -c getcfg.for

gfortran -c getfname.for

gfortran -c getkrat.for

gfortran -c getmodel.for

gfortran -c gettoa.for

gfortran -c getvolt.for

gfortran -c help.for

gfortran -c init0.for

gfortran -c init1.for

gfortran -c init2.for

gfortran -c init3.for

gfortran -c init4.for

gfortran -c init5.for

gfortran -c integral.for

gfortran -c layr.for

gfortran -c layrdens.for

gfortran -c listel.for

gfortran -c mulayelm.for

gfortran -c nedge.for

gfortran -c newline.for

gfortran -c newvolt.for

gfortran -c nextcalc.for

gfortran -c strtthck.for

gfortran -c standard.for

gfortran -c titl.for

gfortran -c printout.for


gfortran gmrfilm.for edge.o absionrt.o omega.o ei.o erfc.o eidp.o effyld.o mu.o trnsprob.o xray.o znl.o layrelem.o qe0.o rjump.o stoich0.o stoich1.o abscoeff.o arfc.o paplimts.o papei.o papex.o papexei.o ppint1.o ppint2.o mac.o rb2a.o bastbeta.o macstd.o charfdta.o papint.o papwt.o pap.o scale.o phirzeq.o papfluor.o tripint.o charflr.o chiov.o ck.o lookup.o wtfract.o compstd.o contflr.o differen.o dlines.o helpfile.o elements.o fixlayer.o fluorprn.o ftest.o getcfg.o getfname.o getkrat.o getmodel.o gettoa.o getvolt.o help.o init0.o init1.o init2.o init3.o init4.o init5.o integral.o layr.o layrdens.o listel.o mulayelm.o nedge.o newline.o newvolt.o nextcalc.o strtthck.o standard.o titl.o printout.o -lm -o gmrfilm.exe

echo 
read -p "press [enter] key to finish..."

