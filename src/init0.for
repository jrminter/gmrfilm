c      
c     initialize variables 5/93 r.a. waldo
c
      subroutine init0(ifix)
      logical ifix(7)
      do 46 kk=1,7
        ifix(kk)=.false.
46    continue
      return
      end
