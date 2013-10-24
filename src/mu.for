c--------------------------------MU----------------------------------
c
c     along with subroutines abscoeff and mac, calculation of
c     mass absorption coefficients
c     for some physical conditions, calculated mac's are uncertain
c     a warning message is printed for these cases
c     10/88 r.a. waldo
c
      subroutine mu(xmu,zx1,zx2,line,ax2)
      character*3 lines(12)
      data lines/'Kb ','Ka ','Lg2','Lb3','Lb4','Lg1',
     &           'Lb1','Lb2','La1','Mg ','Mb ','Ma '/
      ex=xray(zx1,line)
      nza=int(zx2)
      nze=int(zx1)
      if ((nza.eq.40).and.(nze.eq.5)) then
        xmu=8270.
        iflag=2
        goto 1
      endif
      call mac(xmu,nza,ax2,ex,iflag)
      if ((iflag.eq.5).and.(nza.ge.70)) iflag=1
1     if (iflag.eq.1) then
C        print 101,nze,nza,line,xmu
         continue
      else if (iflag.eq.2) then
        print 102,nze,nza,lines(line),xmu
      else if (iflag.eq.3) then
        print 103,nze,nza,lines(line),xmu
      else if (iflag.eq.4) then
        print 104,nze,nza,lines(line),xmu
      else if (iflag.eq.5) then
        print 105,nze,nza,lines(line),xmu
      else if (iflag.eq.6) then
        print 106,nze,nza,lines(line),xmu
      else if (iflag.eq.7) then
        print 107,nze,nza,lines(line),xmu
      else if (iflag.eq.8) then
        print 108,nze,nza,lines(line),xmu
      else if (iflag.eq.9) then
        print 109,nze,nza,lines(line),xmu
      endif
      if (xmu.le.0.) then
9991    print 9998,7,7,7
9998    format(3a1,' MAC is negative; Enter a value for this MAC : ')
        xmu1=xmu
        read(5,*,err=9991)xmu
        print 9999,nze,nza,lines(line),xmu1,xmu
9999    format(' Z(emit)=',i2,';Z(abs)=',i2,'; line=',a3,'; mu changed f
     &rom'  ,1x,f9.1,' to ',f9.1,/)
      endif
1000  continue
101   format('      ok                   ',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
102   format(' !!!  close to edge     !!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
103   format(' !!!  Ec<1.1xcutoff     !!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
104   format(' !!!  Ec below M5 edge  !!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
105   format(' !!!M4>Ec>M5 edge&Zab<70!!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
106   format(' !!!  Ec<M5 & near edge !!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
107   format(' !!!M4>Ec>M5 & near edge!!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
108   format(' !!!   negative  mac    !!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
109   format(' !!!neg. mac & Ec<M5edge!!!',' Z(emit)=',i2
     &,'; Z(abs)=',i2,'; line=',a3,'; mu=',f8.1)
      return
      end
