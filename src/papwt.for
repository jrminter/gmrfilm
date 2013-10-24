c---------------------------------papwt--------------------------------------
c     weighting subroutine for pap parameters
c     10/89  r.a.waldo
c
      subroutine papwt(delta,cnc,c1,nel,nels,L,R)
      real ad(6),delta(7),cnc(15),c1(15),L
      double precision ppint1
      integer nel(7)
      xnorm=1./(ppint1(R,L,R)-ppint1(0.,L,R))
      adtemp=0.
      do 20 j=1,6
        adtemp=adtemp+delta(j)
        ad(j)=adtemp
20    continue
      do 100 j=1,nels
        layer=layr(nel,j)
        if (layer.eq.1) then
          a=0.
          b=amin1(delta(1),r)
          goto 50
        endif
        if (layer.eq.7) then
          a=amin1(ad(6),r)
          b=r
        else
          a=amin1(ad(layer-1),r)
          b=amin1(ad(layer),r)
        endif
50      wt=xnorm*(ppint1(b,L,R)-ppint1(a,L,R))
        c1(j)=cnc(j)*wt
100   continue
      csum=0.
      do 200 j=1,nels
200     csum=csum+c1(j)
      do 250 j=1,nels
250     c1(j)=c1(j)/csum
      return
      end
