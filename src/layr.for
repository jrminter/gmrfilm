c--------------------------------LAYR-----------------------------------
c
c     layr returns the 'layer' only; i.e. layer=1 --> layer 1 is being
c     examined . . . layer=7 --> the substrate is being examined.
c     4/87 r.a.waldo
c
      function layr(nel,j)
      integer sum,nel(7)
      layr=1
      sum=nel(1)
      do 100 i=2,7
        if (j .gt. sum) layr=i
        sum=sum+nel(i)
100   continue
      return
      end
