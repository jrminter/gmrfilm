c------------------------------STANDARD---------------------------------
c
c    calculation of theoretical standard intensities
c    for both pure element and compound standards
c    4/87 r.a. waldo
c    3/88 to include compound standards, r.a. waldo
c
      subroutine standard(nels,symb,ec,e0,line,fconts,fchars
     &,stds0,stds1,stds2,prims,phipar,toa,macchang,icmd8)
      real stds1(15,3,15),ec(15),za(2),z(15),a(15),c(15),
     &prims(15,2),dum(7),mcstd(15,15),ecdum(15),delta(7),
     &fconts(15,2),fchars(15,2),fmac(15,200),exciter(200,3),e0(15)
      character*1 phipar,macchang,icmd8
      integer stds0(15),line(15),std,idum(7),dumline(15)
      logical ff
      character*7 symb(15),symbol,dumsym(15)
      character*2 stds2(15,15),stds(15)
      ff=.false.
      do 100 i=1,nels
        call lookup(symb(i)(1:2),zs,as,ecr,lin)
        c(1)=1.
        dumline(1)=line(i)
        z(1)=zs
        a(1)=as
        ecdum(1)=ec(i)
        symbol=symb(i)
        dumsym(1)=symb(i)
        call macstd(z(1),z(1),a(1),line(i),symb(i)(1:2),
     &  symb(i)(1:2),xmu,macchang,'P')
        mcstd(1,1)=xmu
        idum(1)=1
        lea=nedge(line(i))
        fc1=effyld(zs,lea,e0(i),'electrons')
        fc2=trnsprob(zs,line(i))
        fc3=1./as
        fc4=znl(lea)
        fc5=qe0(ec(i),e0(i),symb(i)(3:5),zs,phipar)
        const=fc1*fc2*fc3*fc4*fc5
        if (phipar.eq.'E') then
           call pap(dum,1,idum,1,z,a,c,e0(i),ec(i),line(i),
     &     toa,mcstd,'B',za,a1,a2,b1,rc,rm,rx,symbol,'E',zz)
           prims(i,1)=za(1)
           goto 10
        endif
        dum(1)=0.
        call phirzeq(idum,1,1,dum,al,b,g,p,z,a,c,
     &  e0(i),ec(i),phipar,line(i),ff,zz)
        call integral(dum,8,al,b,g,p,xmu,dum,prims(i,1),toa)
10      prims(i,1)=const*prims(i,1)
        std=stds0(i)
        fchars(i,1)=0.
        if (icmd8.eq.'Y') then
          call contflr(1,idum,1,z,ec(i),dumline,a,
     &    e0(i),c,fconts(i,1),phipar,toa,mcstd,delta,'B')
        endif
c
c        if a compound standard
c
        if (std.gt.0) then
          l=1
          do 50 ij=1,15
            if (stds1(l,1,std).le.0.) goto 51
            if (symb(i)(1:2).eq.stds2(l,std)) k=l
            c(l)=stds1(l,1,std)
            z(l)=stds1(l,2,std)
            a(l)=stds1(l,3,std)
            l=l+1
            ecdum(l)=0.
50        continue
51        ecdum(k)=ec(i)
          do 70 m=1,l-1
            call macstd(z(k),z(m),a(m),line(i),stds2(k,std),
     &      stds2(m,std),xmu,macchang,'C')
            mcstd(k,m)=xmu
            stds(m)=stds2(m,std)
            dumsym(m)(1:2)=stds(m)
70        continue

          fchars(i,2)=0.
          idum(1)=l-1
          dumline(k)=line(i)
          ecdum(k)=ec(i)
          dumsym(k)=symb(i)
          if (phipar.eq.'E') then
            call pap(dum,k,idum,l-1,z,a,c,e0(i),ec(i),line(i),
     &       toa,mcstd,'B',za,a1,a2,b1,rc,rm,rx,symbol,'C',zz)
          else
            call phirzeq(idum,l-1,k,dum,al,b,g,p,z,a,c,
     &      e0(i),ec(i),phipar,line(i),ff,zz)
            ch=0.
            do 75 j=1,l-1
75            ch=ch+c(j)*mcstd(k,j)
            call integral(dum,8,al,b,g,p,ch,dum,za(1),toa)
          endif
          prims(i,2)=const*c(k)*za(1)
          call charfdta(idum(1),z,dumline,a,ecdum,dumsym,e0,
     &    exciter,fmac,phipar,'C',k)
          call charflr(k,idum,idum(1),'B',z,a,dumsym,
     &    e0(i),c,dum,fchars(i,2),phipar,toa,mcstd,exciter,fmac)
          if (icmd8.eq.'Y') then
            call contflr(k,idum,idum(1),z,ec(i),dumline,a,
     &      e0(i),c,fconts(i,2),phipar,toa,mcstd,delta,'B')
          endif
        else
          prims(i,2)=prims(i,1)
          fchars(i,2)=fchars(i,1)
          fconts(i,2)=fconts(i,1)
        endif
100   continue
      return
      end
