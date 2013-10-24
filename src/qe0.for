c----------------------------QE0--------------------------------
c  Ionization cross section at operating potential
c
c  3/91 r.a. waldo
c  Modified 10/91 according to Pouchou and Pichoir
c  in "Electron Probe Quantitation" Plenum Press (1988 NBS Workshop)
c
      function qe0(xedge,e0,line,z,phipar)
      real mparam
      character*3 line
      character*1 phipar
      u0=e0/xedge
      if ((phipar.eq.'B').or.(phipar.eq.'P')) then
        mparam=.8
      else
        if (line(1:1).eq.'L') mparam=.82
        if (line(1:1).eq.'M') mparam=.78
        if (line(1:1).eq.'K') then
          if (z.gt.30) then
            mparam=.86
          else
            mparam=.86+0.12*exp(-1.*(z/5.)**2.)
          endif
        endif
      endif
c     39229.=pi x e^4 ; e is the electron charge
c     b=.76
      q=alog(u0)/xedge**2/u0**mparam
      if ((phipar.eq.'B').or.(phipar.eq.'P')) then
        qe0=39229.*.76*q
      else if ((phipar.eq.'C').or.(phipar.eq.'E')) then
        if (line(1:1).eq.'K') qe0=1e-20*3.8*mparam*6.023e23*q
        if (line(1:1).eq.'L') qe0=1e-20*5.7*mparam*6.023e23*q
        if (line(1:1).eq.'M') qe0=39229.*mparam*q
      endif
      return
      end
