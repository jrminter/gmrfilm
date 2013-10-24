c------------------------------LAYRDENS-------------------------------
c
c     input of approximate film densities
c     4/87 r.a. waldo
c
      subroutine layrdens(rho,nlayer)
      real rho(7)
      print 990
      print 1
1     format(' Layer densities are used solely to convert thicknesses'
     &,/,' in ug/cm^2 to Angstroms; they have no effect on the results.'
     &)
      print 990
      print 990
      do 100 i=1,nlayer-1
80      print 94,i
        read (5,*,err=80)rho(i)
        if (rho(i) .le. 0.) goto 80
94      format('+Approximate layer ',i1,' density (gm/cm^3): ')
100   continue
      rho(7)=5.
990   format(' ')      
      return
      end
