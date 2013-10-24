c--------------------------------STRTTHCK-------------------------------------
c                             (STaRTing film THiCKnesses)
c
c this subroutine calculates starting film thicknesses for the iteration
c procedure. 9/89, r.a.waldo

      subroutine strtthck(toa,ifix,nel,st,val,c1,c3,ax,e0,nels,delta
     &,kr,mac)
      real c1(15),c3(15),ax(15),delta(7),kr(15),mac(15,15),
     &chiovl(6),e0(15)
      integer nel(7),st(7),val(15)
      logical ifix(7)
      csctheta=1/sin(toa*3.1416/180.)

      call stoich1(ifix,nel,st,val,c1,c3,ax)


      do 120 i=1,nels
        r=6.5e-6*(e0(i)**1.70)
        layer=layr(nel,i)
        if (ifix(layer)) goto 120
        if (st(layer).eq.i) goto 120
        if (layer.ge.2) call chiov(mac,i,nel,layer,c1,chiovl)
        factor=0.
        sumthick=0.
        do 50 j=1,layer-1
          factor=factor+delta(j)*chiovl(j)
          sumthick=sumthick+delta(j)
50      continue
        if (layer.eq.1) then
          delta(1)=delta(1)+r*(1.-sqrt(1.-kr(i)))
          t2=1.
        else if (layer.eq.7) then
          t2=exp(-csctheta*factor)
        else
          t1=sqrt(1.-kr(i)-2.*sumthick/r+((sumthick/r)**2))
          t2=exp(-csctheta*factor)
          delta(layer)=delta(layer)+(r*(1.-t1)-sumthick)
        endif
        c1(i)=c1(i)/t2
120   continue
      return
      end
