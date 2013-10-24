c-------------------------------PAPEXEI-----------------------------
c evaluates final integral when the integral is of the form
c
c       integral = INT [(ax2+bx+c)EXP(r.x)EI(fcp.x)dx]
c
c with approprite limits l1,h1,l2,h2
c calculated in the subroutine "paplimts"
c
c for the PAP quadratic model
c
c 11/90 r.a. waldo
c
      function papexei(r,q,g)
      real a(2),b(2),c(2),lim(2,2)
      real*8 q2,t2,u2,eiu2,eiq2,eu2,et2,rv,w,dr,dq,
     &i1(2),i2(2),i3(2,3),i3a,i3b,i3c,eidp
      common /papparam/lim,a,b,c
      dr=r
      dq=q
      if ((r+q).ne.0.) w=dlog(abs(1+dr/dq))
      ev=exp(-r*g/q)
      p=0.
      p1=0.
      p2=0.
      p3=0.
      h=g/q
      v=r+q
      rv=r/v
      do 100 i=1,2
        if(lim(i,1).eq.1) goto 100
        do 50 j=1,2
          sign=j*2-3
          q2=q*lim(i,j)+g
          if (q2.eq.0.) then
            i1(j)=(-w)/r
            i2(j)=(-rv+w)/r**2-h*i1(j)
            i3(j,1)=-2*w
            i3(j,2)=rv**2+2*rv-h*(2*i2(j)+h*i1(j))*r**3
            i3(j,3)=0.
          else
            t2=r/q*q2
            u2=(r/q+1)*q2
            eiq2=eidp(q2)
            eiu2=eidp(u2)
            et2=dexp(t2)
            eu2=dexp(u2)
            i1(j)=(et2*eiq2-eiu2)/r
            i2(j)=(et2*eiq2*(t2-1)-rv*eu2+eiu2)/r**2-h*i1(j)
c
c due to roundoff errors, the third integral
c must be evaluated in three parts
c
            i3(j,1)=et2*eiq2*(t2**2-2*t2+2)
            i3(j,2)=-rv**2*eu2*(u2-1)+2*rv*eu2-h*(2*i2(j)+h*i1(j))*r**3
            i3(j,3)=-2*eiu2
c
c
c
          endif
50      continue
        i3a=i3(2,1)-i3(1,1)
        i3b=i3(2,2)-i3(1,2)
        i3c=i3(2,3)-i3(1,3)
        p1=p1+(i1(2)-i1(1))*ev*c(i)
        p2=p2+(i2(2)-i2(1))*ev*b(i)
        p3=p3+(i3a+i3b+i3c)*ev/r**3*a(i)
100   continue
      p=p1+p2+p3
      papexei=p
      return
      end
