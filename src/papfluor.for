c------------------------------PAPFLUOR---------------------------
c PAP FLUORescence
c --- -    --  -
c Calculates triple integral for thin filmand bulk samples when phi(rz)
c is a quadratic (PAP model).
c
c 12/90 r.a.waldo
c
      subroutine papfluor(de,mu,chi,ma,mb,ffact,pa1,pa2,pb1,rc,rm,rx
     &,mode)
      real de(9),mu(7),lim(2,2),ap(2),bp(2),cp(2),t(7)
      character*1 mode
      common /papparam/lim,ap,bp,cp
      sumtxmu=0.
      n=sign(1,mb-ma)
      do 2 kk=1,6
2       t(kk)=de(kk+1)-de(kk)
      t(7)=0.
      if (abs(ma-mb).gt.1) then
        do 4 kk=ma+n*1,mb-n*1,n
          sumtxmu=sumtxmu+n*mu(kk)*t(kk)
4       continue
      endif
5     ap(1)=pa1
      ap(2)=pa2
      bp(1)=-2*rm*pa1
      bp(2)=-2*rx*pa2
      cp(1)=pa1*rm**2+pb1
      cp(2)=pa2*rx**2
      d=1./chi
      call paplimts(de(8),de(9),mb,rc,rx,lim)
      uv=alog(abs((mu(ma)+chi)/(mu(ma)-chi)))
      if (mode.eq.'B') then
        a1=-chi
        f1=-mu(mb)
        f2=chi-mu(mb)
       ffact=-d*(papei(f1,0.)-(papexei(a1,f2,0.)+uv*papex(a1)))
        return
      else if (ma.eq.mb) then
        a1=-chi
        b1=chi*de(mb)
        f1=-mu(mb)
        f2=chi-mu(mb)
        g1=mu(mb)*de(mb)
        g2=(-chi+mu(mb))*de(mb)
       ffact=-d*(papei(f1,g1)-exp(b1)*(papexei(a1,f2,g2)+uv*papex(a1)))
        if (mb.eq.7) return
        b2=-chi*t(mb)
        f3=mu(mb)
        f4=(chi+mu(mb))
        g3=-mu(mb)*de(mb+1)
        g4=-(chi+mu(mb))*de(mb+1)
       ffact=ffact+d*(exp(b2)*papei(f3,g3)-exp(b1)*papexei(a1,f4,g4))
        return
      else
        debb1=de(mb)
        if (ma.gt.mb) debb1=de(mb+1)
        a1=-chi*mu(mb)/mu(ma)
        b1=chi/mu(ma)*(mu(mb)*debb1-sumtxmu)-chi*t(ma)
        b2=-chi*t(ma)
        f1=-n*mu(mb)
        f2=mu(mb)*(chi/mu(ma)-n*1)
        g1=n*(mu(mb)*debb1-sumtxmu)-mu(ma)*t(ma)
        g2=-(mu(mb)*debb1-sumtxmu)*(chi/mu(ma)-n*1)
        g3=g2+n*(chi-n*mu(ma))*t(ma)
        g4=n*(mu(mb)*debb1-sumtxmu)
        if (ma.gt.mb) then
          b1=b1+chi*t(ma)
          g1=g1+mu(ma)*t(ma)
          g4=g4-mu(ma)*t(ma)
        endif
       ffact=-d*(papei(f1,g1)+n*exp(b1)*papexei(a1,f2,g2))
       if (ma.eq.7) return
       ffact=ffact+d*(n*exp(b1)*papexei(a1,f2,g3)+exp(b2)*papei(f1,g4))
        return
      endif
      end
