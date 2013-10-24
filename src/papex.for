c-------------------------------PAPEX-----------------------------
c evaluates final integral when the integral is of the form
c
c       integral = INT [(ax2+bx+c)EXP(fcp.x)dx]
c
c with approprite limits l1,h1,l2,h2
c calculated in the subroutine "paplimts"
c
c for the PAP quadratic model
c
c 11/90 r.a. waldo
c
      function papex(r)
      real a(2),b(2),c(2),lim(2,2)
      real*8 i1(2),i2(2),i3(2),ru,ru2
      common /papparam/lim,a,b,c
      p1=0.
      p2=0.
      p3=0.
      do 100 i=1,2
        if(lim(i,1).eq.1) goto 100
        do 50 j=1,2
          ru=r*lim(i,j)
          ru2=dexp(ru)
          i1(j)=ru2
          i2(j)=ru2*(ru-1)
          i3(j)=ru2*(ru**2-2*ru+2)
50      continue
        p1=p1+(i1(2)-i1(1))*c(i)/r
        p2=p2+(i2(2)-i2(1))*b(i)/r**2
        p3=p3+(i3(2)-i3(1))*a(i)/r**3
100   continue
      p=p1+p2+p3
      papex=p
      return
      end
