c-------------------------------PAPEI-----------------------------
c evaluates final integral when the integral is of the form
c
c       integral = INT [(ax2+bx+c)EI(f.x+g)dx]
c
c with approprite limits l1,h1,l2,h2
c calculated in the subroutine "paplimts"
c
c This is for the PAP quadratic model
c
c 11/90 r.a. waldo
c
      function papei(f,g)
      real a(2),b(2),c(2),lim(2,2)
      real*8 i1(2),i2(2),i3(2),u2,eiu2,eu2,eidp
      common /papparam/lim,a,b,c
      p1=0.
      p2=0.
      p3=0.
      do 100 i=1,2
        if(lim(i,1).eq.1) goto 100
        do 50 j=1,2
          u2=f*lim(i,j)+g
          if (u2.eq.0.) then
            eiu2=1.
          else
            eiu2=eidp(u2)
          endif
          eu2=dexp(u2)
          h=g/f
          i1(j)=(u2**1*eiu2-eu2)/f
          i2(j)=(u2**2*eiu2-eu2*(u2-1.))/(2*f**2)-h*i1(j)
          i3(j)=(u2**3*eiu2-eu2*(u2**2-2*u2+2))/(3*f**3)-
     &          h*(2*i2(j)+h*i1(j))
50      continue
        p1=p1+(i1(2)-i1(1))*c(i)
        p2=p2+(i2(2)-i2(1))*b(i)
        p3=p3+(i3(2)-i3(1))*a(i)
100   continue
      p=p1+p2+p3
      papei=p
      return
      end
