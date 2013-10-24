c----------------------------------DIFFEREN-----------------------------------
c                              ( difference)
c  Analysis of an element by difference, bulk analysis only.
c  This subroutine calculates the weight fraction concentration of the
c  element analyzed by difference.
c  9/89, r.a. waldo
c
      Subroutine differen(c1,symb,nels)
      real c1(15)
      logical bydiff
      character*7 symb(15)
      bydiff=.false.
      sum=0.
      do 10 i=1,nels
        if (symb(i)(7:7).eq.'D') bydiff=.true.
10    continue
      if (.not.bydiff) return
      do 100 i=1,nels
        if (symb(i)(7:7).eq.'D') then
        j=i
        else
          sum=sum+c1(i)
        endif
100   continue
      c1(j)=1.-sum
      if (c1(j).le.0.) c1(j)=1.e-5
      return
      end
