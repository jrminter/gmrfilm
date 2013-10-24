c-------------------------------SCALE------------------------------------
c
c     for thin films, the parameter values used in the phi(rz) equation
c     to calculate x-ray intensity will depend on layer thickness;
c     substrate parameter values will predominate for thin films,
c     layer values will predominate for thicker films
c
c     weightings of the various layers and substrate are calculated
c     phi(0) and beta weights are based on erfc(2A*D)
c     [COMP.ERROR.FTN(2*ALPHA*DELTA)]
c     while alpha and beta are apportioned based on   'erfc(A*D)'
c     the difference is due to the backscattering effect on phi0
c     program completed 4/87 by richard a. waldo
c
      subroutine scale(delta,alpha,lwt)
      real delta(7),lwt(14)
      if (delta(1).eq.0.) then
        lwt(1)=1.
        lwt(8)=1.
        return
      endif
      d=0.
      wtsum1=0.
      wtsum2=0.
      do 100 i=1,6
        d1=d+delta(i)
        if (delta(i).gt.0.) then
          lwt(i)=erfc(alpha*d)-erfc(alpha*d1)
          lwt(i+7)=erfc(2.*alpha*d)-erfc(2.*alpha*d1)
        else
          lwt(i)=0.
          lwt(i+7)=0.
        endif
        wtsum1=wtsum1+lwt(i)
        wtsum2=wtsum2+lwt(i+7)
        d=d1
100   continue
      lwt(7)=1.-wtsum1
      lwt(14)=1.-wtsum2
      return
      end
