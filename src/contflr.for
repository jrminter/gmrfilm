c-----------------------------CONTFLR-------------------------------
c
c     This program controls the calculation of the CONTinuum
c     x-ray FLuoRescence correction for thin film and bulk system
c     standards and specimens.
c     program completed on 3/91 by richard a. waldo
c
      subroutine contflr(na,nel,nels,zx,eci,line,ax,
     &e0,cnc,fcont,phipar,toa,mac2,delta,mode)
      real zx(15),ax(15),cnc(15),mac2(15,15),exciter1(50),m
     &,delta(7),chdum(6),delta2(9),za(2),contedge(50),fcp(7)
     &,fcs(7),cmac(15,100,5)
      integer line(15),nel(7)
      character*1 phipar,mode
      character*3 edges(12)
      character*7 symbol
      logical ff,exciter2(50)
      data edges/'K  ','Lb3','Lb1','La1','M1 ','M2 ',
     &           'Mg ','Mb ','Ma ','N1 ','N2 ','N3 '/
      fcont=0.
      symbol='ContFlr'
      csctheta=1/sin(toa/57.29578)
      if (mode.eq.'B') then
        layers=1
      else
        layers=7
      endif
      delta2(1)=0.
      if (mode.eq.'B') then
        delta2(8)=0.
        delta2(9)=rx
        delta(7)=1.
      else
        temp=0.
        do 10 kk=2,7
          temp=temp+delta(kk-1)
          delta2(kk)=temp
10      continue
      endif
      layer=layr(nel,na)
      do 30 ii=1,layers
        fcs(ii)=0.
30    continue
      do 40 ik=1,nels
        layerc=layr(nel,ik)
        fcs(layerc)=fcs(layerc)+mac2(na,ik)*cnc(ik)*csctheta
40    continue
      s1=0.  
      
      lea=nedge(line(na))
      if (zx(na).le.10.) return
      if ((lea.gt.1).and.(zx(na).le.40.)) return
      if ((lea.gt.4).and.(zx(na).le.60.)) return
      do 100 i=1,50
        exciter2(i)=.true.
100   continue
      l=2
      do 500 j=1,nels
        do 400 k=1,12
          xedge=edge(int(zx(j)),edges(k))
          if ((xedge.gt.eci).and.(xedge.lt.e0)) then
            exciter1(l)=xedge
            l=l+1
          endif
400     continue
500   continue
c
c sort the edges by energy -- high to low
c
      exciter1(1)=e0
      exciter1(l)=eci
      do 520 m1=1,l
        temp=0.
        do 510 m2=1,l
          if ((exciter2(m2)).and.(exciter1(m2).gt.temp)) then
            n=m2
            temp=exciter1(m2)
          endif
510     continue
        contedge(m1)=exciter1(n)
        exciter2(n)=.false.
520   continue
      nn=l
      rx=1.5*6.5*(e0**1.7)/1.e6
      delta2(1)=0.
      delta2(8)=0.
      fc1=trnsprob(zx(na),line(na))
      constant=1.0e-8*exp(-.0322*e0+5.8)
      M=.00599*e0+1.05
      C=6*10e-10
      do 5000 nlb=1,nn-1
        s1=0.
        fc2=absionrt(zx(na),lea,contedge(nlb)-1.e-3)
        fc3=effyld(zx(na),lea,contedge(nlb)-1.e-3,'xrays    ')
        xh=contedge(nlb)
        xl=contedge(nlb+1)
        xmid=(xh+xl)/2.
        if (xh-xl.lt.2.) then
          xenergy=(xh+xl)/2.
          do 730 ik=1,nels
            call mac(xmu,int(zx(ik)),ax(ik),xenergy,iflag)
            if (xmu.gt.20000.) xmu=20000.
            cmac(ik,nlb,1)=xmu
730       continue
          
          dx=3.*(xh-xl)
          intervls=0
          xh=xmid
        
        
        else
          intervls=4
          dx=(xh-xl)/intervls
          do 800 i=1,intervls+1
            xenergy=xh-(i-1)*dx
            do 750 ik=1,nels
              if (i.eq.1) then
                call mac(xmu,int(zx(ik)),ax(ik),xenergy-1.e-3,iflag)
              else if (i.eq.intervls+1) then
                call mac(xmu,int(zx(ik)),ax(ik),xenergy+1.e-3,iflag)
              else
                call mac(xmu,int(zx(ik)),ax(ik),xenergy,iflag)
              endif
              if (xmu.gt.20000.) xmu=20000.
              cmac(ik,nlb,i)=xmu
750         continue
800       continue
          
        endif
        if (phipar.eq.'E') then
          call pap(delta,16,nel,nels,zx,ax,cnc,e0,xmid,2,
     &    toa,mac2,'B',za,a1,a2,b1,rc2,rm,rx,symbol,'B',z)
        else
          call phirzeq(nel,nels,16,delta,alpha,beta,gamma,phi0
     &    ,zx,ax,cnc,e0,xmid,phipar,2,ff,z)
          call integral (delta,8,alpha,beta,gamma,
     &                 phi0,0.,chdum,za(2),toa)
        endif
        delta2(9)=rx
        ffact=0.
        do 4000 i=1,intervls+1
          xenergy=xh-(i-1)*dx
          if (xenergy.gt.e0-1.e-3) goto 4000
          do 410 ii=1,layers
410         fcp(ii)=0.
          do 420 ik=1,nels
            layerc=layr(nel,ik)
            xmu=cmac(ik,nlb,i)
            fcp(layerc)=fcp(layerc)+cnc(ik)*xmu
420       continue
           ffactor=0.
           fcsp=cmac(na,nlb,i)
           ffact=0.
           do 3000 layerb=1,layers
              if (nel(layerb).eq.0) goto 3000
              if (mode.eq.'F') then
                if (layerb.eq.7) then
                  delta2(8)=delta2(7)
                  delta2(9)=rx
                else
                  delta2(8)=delta2(layerb)
                  delta2(9)=amin1(delta2(layerb+1),rx)
                endif
              endif
              if (delta2(8).ge.delta2(9)) goto 3000
              if (phipar.eq.'E') then
                call papfluor(delta2,fcp,fcs(layer),layer,
     &          layerb,ffactor,a1,a2,b1,rc2,rm,rx,mode)
              else
                call tripint(delta2,fcp,fcs(layer),layer,
     &          layerb,ffactor,alpha,beta,gamma,phi0,mode)
              endif
              factor=0.
              if ((mode.eq.'B').or.(layer.eq.1)) goto 771
              do 770 kk=2,layer
770             factor=factor+fcs(kk-1)*delta(kk-1)
771           ffactor=ffactor*exp(-1.*amin1(factor,88.))
              ffact=ffact+ffactor
3000        continue
          f1=2.
          if ((i.eq.1).or.(i.eq.intervls+1)) f1=1.
          if ((int(i/2.)-i/2.).eq.0.) f1=4.
          s1=s1+fcsp*ffact/za(2)*
     &       (constant*(z*(e0/xenergy-1.))**M+C*z**2)*f1
4000    continue
        fcont=fcont+(s1*.5*fc1*cnc(na)*fc2*fc3)*dx/3.
5000  continue
      return
10000 end
