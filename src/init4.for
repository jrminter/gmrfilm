c      
c     initialize variables 5/93 r.a. waldo
c
      subroutine init4(fconts,fchars,prims)
      real fconts(15,2),fchars(15,2),prims(15,2)
      do 10 k=1,15
        do 10 j=1,2
          fconts(k,j)=0.
          fchars(k,j)=0.
          prims(k,j)=0.
10     continue
       return
       end
