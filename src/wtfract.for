c-----------------------------WTFRACT---------------------------------
c
c     a subroutine to convert atomic formula
c     to atomic percent and weight percent
c     4/87 raw.
c
      subroutine wtfract(symb,form,nl,cnc)
      real wtpc(15),form(15),cnc(15)
      integer nl(3)
      character*7 symb(15)
      l=0
      m=0
      do 300 i=1,7
        sumwp=0.
        j=nl(i)
        if (j.eq.0) goto 300
        do 100 k=1,j
          l=l+1
          call lookup(symb(l)(1:2),z,a,ecr,lin)
          sumwp=sumwp+a*form(l)
          wtpc(l)=a*form(l)
100     continue
        do 200 n=1,j
          m=m+1
          cnc(m)=wtpc(m)/sumwp
200     continue
300   continue
      return
      end
