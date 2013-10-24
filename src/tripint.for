c-------------------------------TRIPINT---------------------------------
c     Fluorescent TRIPle INTegral for bulk and film systems, coupled
c     with fluor and flrdata, calculates the fluorescence factor for
c     bulk and film systems.
c     The integral is calculated with simpson's rule.
c
c     completed 6/88 by r.a. waldo.
c
      subroutine tripint(de,mu,chi,ma,mb,ffact,al,beta,g,p,mode)
      real de(9),mu(7),t(6)
      character*1 mode
      sumtxmu=0.
      do 2 kk=1,6
        t(kk)=de(kk+1)-de(kk)
2     continue
      if (abs(ma-mb).gt.1) then
        i1=ma+1
        i2=mb-1
        if (ma.gt.mb) then
          i1=mb+1
          i2=ma-1
        endif
        do 4 kk=i1,i2
          sumtxmu=sumtxmu+mu(kk)*t(kk)
4       continue
      endif
      n=13
      xincb=(de(9)-de(8))/(1.*(n-1))
      z0=de(8)-xincb
      sum=0.
c next line is to protect against ei(0) and v=ln(0) errors
      if (chi.eq.mu(ma)) mu(ma)=.999*chi
      d=1/chi
      u=alog(1.+chi/mu(ma))
      v=alog(abs(1.-chi/mu(ma)))
      uv=alog(abs((mu(ma)+chi)/(mu(ma)-chi)))
      do 1000 l=1,n
        z0=z0+xincb
        trm1=(al*z0)**2
        if (trm1.gt.88.) then
          sint=0.
          goto 999
        endif
        trm2=amin1(88.,trm1+beta*z0)
        phi=g*(exp(-trm1))-(g-p)*(exp(-trm2))
        if ((ma.eq.7).and.(mb.eq.7).and.(l.eq.1)) then
        endif
        if (mode.eq.'B') then
          if (z0.le.0.) then
            sint=u
          else
            a1=-chi
            f1=-mu(1)
            f2=chi-mu(1)
           term1=-ei(f1*z0)
           term2=exp(a1*z0)*ei(f2*z0)
           term3=exp(a1*z0)*uv
           sint=term1+term2+term3
          endif
          goto 999
        else if (ma.eq.mb) then
          if (z0.le.de(ma)+1.e-8) then
            if (ma.eq.7) then
              sint=u
            else
              b1=-amin1(chi*t(ma),88.)
              g1=-mu(ma)*t(ma)
              g2=-(chi+mu(ma))*t(ma)
              sint=exp(b1)*ei(g1)-ei(g2)+u
            endif
            goto 999
          else if ((z0.ge.de(ma+1)-1.e-8).and.(ma.ne.7)) then
            b1=-amin1(chi*t(ma),88.)
            g1=-mu(ma)*t(ma)
            g2=(chi-mu(ma))*t(ma)
            sint=-ei(g1)+exp(b1)*(ei(g2)-v)
            goto 999
          else
            a1=-chi
            b1=chi*de(ma)
            f1=-mu(ma)
            f2=chi-mu(ma)
            g1=mu(ma)*de(ma)
            g2=(-chi+mu(ma))*de(ma)
           term1=-ei(f1*z0+g1)
           term2=exp(a1*z0+b1)*ei(f2*z0+g2)
           term3=exp(a1*z0+b1)*uv
           sint=term1+term2+term3
            if (ma.lt.7) goto 100
            if (ma.eq.7) goto 999
100         b2=-chi*t(ma)
            f3=mu(ma)
            f4=chi+mu(ma)
            g3=-mu(ma)*de(ma+1)
            g4=-(chi+mu(ma))*de(ma+1)
           term4=exp(b2)*ei(f3*z0+g3)
           term5=-exp(a1*z0+b1)*ei(f4*z0+g4)
            sint=sint+term4+term5
            goto 999
          endif
        else if (ma.lt.mb) then
          if ((de(ma+1).eq.de(mb)).and.(z0.le.de(mb)+1.e-8)) then
            b1=-amin1(chi*t(ma),88.)
            g1=-mu(ma)*t(ma)
            g2=(chi-mu(ma))*t(ma)
            sint=-ei(g1)+exp(b1)*(ei(g2)-v)
            goto 999
          else
            a1=-chi*mu(mb)/mu(ma)
            b1=chi/mu(ma)*(mu(mb)*de(mb)-sumtxmu)-chi*t(ma)
            b2=-chi*t(ma)
            f1=-mu(mb)
            f2=mu(mb)*(chi/mu(ma)-1)
            g1=mu(mb)*de(mb)-sumtxmu-mu(ma)*t(ma)
            g2=-(mu(mb)*de(mb)-sumtxmu)*(chi/mu(ma)-1)
            g3=g2+(chi-mu(ma))*t(ma)
            g4=mu(mb)*de(mb)-sumtxmu
           term1=-ei(f1*z0+g1)
           term3=exp(a1*z0+b1)*ei(f2*z0+g3)
           term2=-exp(a1*z0+b1)*ei(f2*z0+g2)
           term4=exp(b2)*ei(f1*z0+g4)
           sint=term1+term2+term3+term4
          endif
        else if (ma.gt.mb) then
          if (ma.lt.7) then
            if ((de(mb+1).eq.de(ma)).and.(z0.ge.de(mb+1)-1.e-8)) then
              b1=-amin1(chi*t(ma),88.)
              g1=-mu(ma)*t(ma)
              g2=-(chi+mu(ma))*t(ma)
              sint=exp(b1)*ei(g1)-ei(g2)+u
              goto 999
            else
              a1=-chi*mu(mb)/mu(ma)
              b1=chi/mu(ma)*(mu(mb)*de(mb+1)+sumtxmu)
              b2=-chi*t(ma)
              f1=mu(mb)*(chi/mu(ma)+1)
              f2=mu(mb)
              g1=-(chi/mu(ma)+1)*(mu(mb)*de(mb+1)+sumtxmu)-
     &            (chi+mu(ma))*t(ma)
              g2=-(mu(mb)*de(mb+1)+sumtxmu)-mu(ma)*t(ma)
              g3=-(chi/mu(ma)+1)*(mu(mb)*de(mb+1)+sumtxmu)
              g4=-(mu(mb)*de(mb+1)+sumtxmu)
             term1=exp(b2)*ei(f2*z0+g2)
             term2=-exp(a1*z0+b1)*ei(f1*z0+g1)
             term3=-ei(f2*z0+g4)
             term4=exp(a1*z0+b1)*ei(f1*z0+g3)
             sint=term1+term2+term3+term4
             goto 999
            endif
          else
            if ((de(mb+1).eq.de(ma)).and.(z0.ge.de(mb+1)-1.e-8)) then
              sint=u
              goto 999
            else
              a1=-chi/mu(ma)*mu(mb)
              b1=chi/mu(ma)*(mu(mb)*de(mb+1)+sumtxmu)
              f1=(chi/mu(ma)+1)*mu(mb)
              f2=mu(mb)
              g1=-(chi/mu(ma)+1)*(mu(mb)*de(mb+1)+sumtxmu)
              g2=-(mu(mb)*de(mb+1)+sumtxmu)
             term1=exp(a1*z0+b1)*ei(f1*z0+g1)
             term2=-ei(f2*z0+g2)
             sint=term1+term2
             goto 999
            endif
          endif
        endif
999     f1=2.
        if ((l.eq.1).or.(l.eq.n)) f1=1.
        if ((int(l/2.)-l/2.).eq.0.) f1=4.
        sum=sum+sint*phi*f1
1000  continue
      ffact=sum*d*xincb/3.
      return
      end
