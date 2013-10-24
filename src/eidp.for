c---------------------------------EIDP--------------------------------
c
c     Exponential Integral, Double Precision
c     This function calculates the exponential integral ei(x)
c     with 8 byte precision for both positive and negative values of x.
c     11/90 r.a. waldo
c
      function eidp(x1)
      real*8 eidp,x1,x,sign,xiter,t
      x=x1
      sign=1.
      if (x.lt.0.) sign=-sign
      if (x.lt.0.) x=-x
      xiter=1.
      t=sign*x
      eidp=.5772156649+dlog(x)+sign*x
1     xiter=xiter+1.
      t=sign*x*t*(xiter-1)/xiter/xiter
      eidp=eidp+t
      if (abs(t/eidp).lt.1.e-12) return
      goto 1
      end
