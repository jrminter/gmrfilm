c------------------------------LAYRELEM---------------------------------
c
c     LAYeR ELEMent returns the 'layer' i.e., layer=1 --> layer is being
c     examined . . . layer=7 --> substrate is being examined. it also returns
c     'ne' the number of the element examined in the layer or substrate.
c     4/87 r.a. waldo
c
      subroutine layrelem(ne,nel,layer,j)
      integer sum,nel(7)
      layer=1
      ne=j
      sum=nel(1)
      do 100 i=2,7
        if (j .gt. sum) then
          layer=i
          ne=j-sum
        endif
        sum=sum+nel(i)
100   continue
      return
      end
