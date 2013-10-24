c------------------------------TITL---------------------------------
c
c     'titl' creates a title to be used in data output based on the
c     alphanumeric representations (atomic symbols) of all the
c     analyzed elements.
c     4/87 r.a. waldo. revised 1/90, raw.
c
      subroutine titl(nel,nels,symb,e0,ifix,finame,phipar,iter,
     &ianalys,mode,standrd,stds0,cstd,val,mle,cfg)
      real wtfrel(6),cstd(15),e0(15)
      logical ifix(7)
      integer nel(7),stds0(15),val(15),mle(7)
      character*30 mydate
      character*3 month(12)
      character*5 standrd(15),stdname
      character*7 symb(15)
      character*12 label1,labelq,labelnq,finame
      character*2 el(6),ianly,iiter,ie02
      character*4 ie04
      character*76,title,voltages
      character*1 phipar,mode,ilayer
      character*1 cfg(10)
      data month/'Jan','Feb','Mar','Apr','May','Jun','Jul','Aug',
     &'Sep','Oct','Nov','Dec'/
      labelq(1:12)= '  "      :  '
      labelnq(1:12)='         :  '
      lu1=6
      lu2=7
      if (cfg(6).eq.'P') then
        lu1=5
        open (unit=5,file='LPT1')
      endif
      open (unit=7,file=finame,status='unknown')
      endfile (7)
      backspace (7)
      call fdate(mydate)
c      call getdat(iy,im,id)
      write(ianly,fmt='(i2)')ianalys
      if (cfg(5).eq.'Y') then
        if (amod(e0(1),1.).eq.0.) then
          ie01=int(e0(1))
          write(ie02,fmt='(i2)')ie01
          title(1:23)='Analysis #'//ianly//' at '//ie02//'kV;  '
        else
          write(ie04,fmt='(f4.1)')e0(1)
          title(1:23)='Analysis #'//ianly//' at '//ie04//'kV;'
        endif
      else
          title(1:23)='Analysis #'//ianly//';          '
      endif
      if (phipar.eq.'B') then
        title(24:66)=' Bastin''s Scanning (1986) Phi(rz) Model;  '
      else if (phipar.eq.'C') then
        title(24:66)=' Bastin''s Scanning (1990) Phi(rz) Model;  '
      else if (phipar.eq.'E') then
        title(24:66)=' Pouchou,Pichoir''s Scanning (1990) Model; '
      else if (phipar.eq.'P') then
        title(24:66)=' Packwood''s MAS (1986) Phi(rz) Model;     '
      endif
      write(iiter,fmt='(i2)')iter
      title(67:76)=' iter.='//iiter//' '
      if ((cfg(1).eq.'Y').or.(cfg(2).eq.'Y')) then
        title(14:15)=', '
        title(16:26)=title(66:76)
        title(27:76)=' '
        do 5 lu=lu1,lu2
5         write (lu,920) title(1:76)
        goto 215
      endif
      if (cfg(5).eq.'N') then
        ll=1
        do 8 jj=1,nels
          ie01=int(e0(jj))
          write(ie02,fmt='(i2)')ie01
          voltages(ll:ll+4)=symb(jj)(1:2)//ie02//' '
          ll=ll+5 
8       continue            
      endif
      do 10 lu=lu1,lu2
        if (mode.eq.'F') write (lu,900) finame,id,month(im),mod(iy,100)
        if (mode.eq.'B') write (lu,902) finame,id,month(im),mod(iy,100)
        write (lu,905)
        write (lu,920) title(1:76)
        if (iter.ge.12) write (lu,925) iter
        if (cfg(5).eq.'N') then
          write(lu,9260)
          write(lu,920) voltages(1:76)
        endif
        if (cfg(8).eq.'Y') write (lu,9261)
        if (cfg(8).ne.'Y') write (lu,9262)
        write (lu,970)
10    continue
      n=7
      ll=1
      if (mode.eq.'B') n=1
      do 50 k=1,n
        if (mode.eq.'B') then
          label1(1:12)='Elements :  '
        else
          if (k.eq.1) then
            label1(1:12)='Layer 1  :  '
            goto 14
          endif
          ll=ll+nel(k-1)
          label1(1:6)='Layer '
          label1(8:12)='  :  '
          write(ilayer,fmt='(i1)')k
          if (k.eq.7) then
            label1(1:12)='Substrate:  '
          else
            label1(7:7)=ilayer
          endif
        endif
14      if (nel(k).eq.0) goto 50
        lu=min0(8,nel(k))
        call listel(title,label1,ll,ll+lu-1,symb)
        do 15 lu=lu1,lu2
15        write (lu,920) title(1:76)
        if (nel(k).gt.9) then
          call listel(title,labelq,ll+8,ll+nel(k)-1,symb)
          do 20 lu=lu1,lu2
20          write (lu,920) title(1:76)
        endif
        if ((ifix(k)).and.(cfg(3).eq.'C').and.(mode.eq.'F')) then
          do 25 lu=lu1,lu2
            if (k.lt.7) write (lu,960) labelq
            if (k.eq.7) write (lu,965) labelq
25        continue
          goto 50
        endif
        do 40 m=ll,ll+nel(k)-1
          if (symb(m)(7:7).eq.'S') then
            if (mode.eq.'B') then
              do 30 lu=lu1,lu2
                write (lu,915) labelnq,symb(m)(1:2),'stoichiometry.'
                write (lu,950) labelnq,(symb(j)(1:2),j=ll,ll+nel(k)-1)
30              write (lu,955) labelnq,(val(j),j=ll,ll+nel(k)-1)
            else if (mode.eq.'F') then
              do 35 lu=lu1,lu2
                write (lu,915) labelq,symb(m)(1:2),'stoichiometry.'
                write (lu,950) labelq,(symb(j)(1:2),j=ll,ll+nel(k)-1)
                write (lu,955) labelq,(val(j),j=ll,ll+nel(k)-1)
35            continue
            endif
          endif
40      continue
c        if (mode.eq.'F') then
c          do 45 lu=lu1,lu2
c            write (lu,970)
c45        continue
c        endif
50    continue
      if (mode.eq.'B') then
        do 60 m=1,nels
          if (symb(m)(7:7).eq.'D') then
            do 55 lu=lu1,lu2
55            write (lu,915) labelnq,
     &        symb(m)(1:2),'difference.   '
          endif
60      continue
      endif
      if (cfg(7).eq.'Y') then
        do 62 lu=lu1,lu2
          write (lu,927) symb(mle(1))(1:2),layr(nel,mle(1))
62      continue
      endif
      open (unit=2,file='standard.dat',status='unknown')
      do 100 j=1,nels
      if ((symb(j)(7:7).eq.'C').or.(symb(j)(7:7).eq.'N')) then
        do 70 lu=lu1,lu2
          write (lu,930)
70        write (lu,935)
        goto 105
      endif
100   continue
105   do 200 i=1,nels
        l2=layr(nel,i)
        if ((symb(i)(7:7).ne.'C').and.(symb(i)(7:7).ne.'N')) goto 200
        ics=stds0(i)
110     read (2,120,end=150)stdname,nl
120     format(a5,1x,i2)
        if (standrd(ics).ne.stdname) goto 110
        read (2,130)(el(j),wtfrel(j),j=1,nl)
130     format(15(a2,f8.5,3x):)
        do 140 lu=lu1,lu2
140       write (lu,940) symb(i)(1:2),symb(i)(3:5)
     &    ,standrd(ics),1./cstd(i),(el(j),wtfrel(j),j=1,nl)
        rewind (2)
        goto 200
150   do 160 lu=lu1,lu2
160       write (lu,945)
200   continue
      rewind (2)
      close (unit=2,status='keep')
      do 210 lu=lu1,lu2
210     write (lu,970)
      close (unit=5)
215   return
900   format(//' GMR Electron Probe Thin Film Microanalysis   File:',a12
     &,4x,i2,'-',a3,'-',i2)
902   format(//' GMR Electron Probe Bulk Microanalysis        File:',a12
     &,4x,i2,'-',a3,'-',i2)
905   format(' =========================================================
     &=================='/)
915   format(' ',a12,a2,' is analyzed by ',a14)
920   format(' ',a76)
925   format(' !!!Warning!!!, Iterations>',i2,', Can specimen be treated
     & with bulk phi(rz) model?')
9260  format(' Operating Potentials:')
9261  format(' Continuum Correction Included')
9262  format(' Continuum Correction Not Included')
927   format(/' Element ''',a2,''' in layer ',i1, ' has been determined
     &by apportioning',/,' the experimental k-factors among the layers.
     &')
930   format(/'----COMPOUND STANDARDS---- ')
935   format (' El.,  Std.  k(std/el)  Standard Elements and Weight Frac
     &tions',/,' Line  Name   (Theor.)',/,' ====  ====  =========  =====
     &===============================================')
940   format(1x,a2,a3,1x,a5,3x,f5.4,3x,6(1x,a2,1x,f5.4):)
945   format (' error, standard not found ')
950   format(' ',a12,'Element:',15(2x,a2):)
955   format(' ',a12,'Valence:',15(1x,i3):)
960   format(' ',a12,'The composition and thickness of this layer have b
     &een fixed.'/)
965   format(' ',a12,'The composition of the substrate has been fixed.'/
     &)
970   format(' ')
      end
