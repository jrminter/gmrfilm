c-------------------------------PAPLIMTS----------------------------------
c
c     PAP LIMiTS
c     --- --- --
c     This subroutine determines the limits of integration for primary and
c     secondary x-ray intensities.  Because the PAP model uses two quadratic
c     functions to approximate the x-ray depth distribution, the integration
c     could be one of three cases;
c       1) with limits in phi(a) only
c       2)  "     "    "  phi(b)  "
c       3)  "   lower limit in phi(a), upper limit in phi(b)
c
c     program completed 11/90 by richard a. waldo
c
      subroutine paplimts(ad1,ad2,icase,rc,rx,lim)
      real lim(2,2)
c
c lim(1,1)= lower limit phi function 1
c lim(1,2)= upper limit phi function 1
c lim(2,1)= lower limit phi function 2
c lim(2,2)= upper limit phi function 2
c
        lim(1,1)=0.e-6
        lim(1,2)=rc
        lim(2,1)=rc
        lim(2,2)=rx
      if (icase.eq.8) return
      if (icase.eq.1) then
        if (rc.gt.ad2) then
          lim(1,2)=ad2
          lim(2,1)=1.
        else if (ad2.lt.rx) then
          lim(2,2)=ad2
        endif
      else if (icase.lt.7) then
        if (rc.lt.ad1) then
          lim(1,1)=1.
          lim(2,1)=ad1
          if (ad2.lt.rx) lim(2,2)=ad2
        else
          lim(1,1)=ad1
          if (rc.gt.ad2) then
            lim(1,2)=ad2
            lim(2,1)=1.
          else
            if (ad2.lt.rx) lim(2,2)=ad2
          endif
        endif
      else if (icase.eq.7) then
        if (rc.lt.ad1) then
          lim(1,1)=1.
          lim(2,1)=ad1
        else
          lim(1,1)=ad1
        endif
      endif
      if (lim(2,1).gt.rx) lim(2,1)=1.
999   return
      end
