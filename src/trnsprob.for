c------------------------------TRNSPROB-----------------------------------
c TRaNSition PROBability of line after shell ionization.
c
c
      function trnsprob(z,k)
      goto(1,1,2,3,4,5,5,6,6,7,7,7),k
1     if (z.lt.11.) then
        trnsprob=.01
      else
        trnsprob= 8.e-3*(z-10)-8.75e-5*(z-10)**2
      endif
      if (k.eq.2) trnsprob=1.-trnsprob
      return
2     trnsprob=.25
      return
3     trnsprob=.45
      return
4     trnsprob=.3
      return
5     trnsprob=(z-60.)*.001+.14
      if(k.eq.7) trnsprob=1.-trnsprob
      return
6     trnsprob=(z-60.)*.001+.16
      if(k.eq.9) trnsprob=1.-trnsprob
      return
7     trnsprob=1.0
      return
      end
