
c------------------------------PRINTOUT---------------------------------
c
c     this subroutine outputs results to terminal/printer
c     4/87 r.a.waldo
c     revised 3/88 for compound standards and
c     analysis by stoichiometry, r.a. waldo
c     revised 6/88 for fluorescence correction, r.a. waldo
c
      subroutine printout(ifix,layrchar,nel,nels,c1,ax,prim,
     &fcont,delta,symb,rho,fchar,kf,kr,krx,cfg,mode,
     &prims,fchars,fconts)
      real c1(15),ax(15),delta(7),sum(7,2),rho(7),kr(15),kf(15),kt
     &,krx(15),atfrden(7),atfr(15),fchar(15),prim(15),fcont(15)
     &,prims(15,2),fchars(15,2),fconts(15,2),ffcont(15),ffchar(15)
      character symb(15)*7,layrchar(7)*10
      logical ifix(7)
      character*1 mode,cfg(10)
      integer nel(7)
      delta(7)=5.
      endfile(7)
      backspace (7)
      lu1=6
      lu2=7
      if (cfg(6).eq.'P') then
        lu1=5
        open (unit=5,file='LPT1')
      endif
      do 3 i=1,nels
         ffcont(i)=(1.-((prim(i)+fchar(i))/(prims(i,1)+fchars(i,1)))
     &   /((prim(i)+fchar(i)+fcont(i))/
     &   (prims(i,1)+fchars(i,1)+fconts(i,1))))*100.
         ffchar(i)=(1.-((prim(i)+fcont(i))/(prims(i,1)+fconts(i,1)))
     &   /((prim(i)+fchar(i)+fcont(i))/
     &   (prims(i,1)+fchars(i,1)+fconts(i,1))))*100.
3     continue
      if (mode.eq.'B') goto 1000
      do 10 lu=lu1,lu2
        write (lu,910)
        if (cfg(3).eq.'K') write (lu,920)
        if (cfg(3).eq.'C') write (lu,930)
        write (lu,940)
10    continue
      do 25 i=1,7
        do 20 j=1,2   
20       sum(i,j)=0.
25       atfrden(i)=0.
      do 30 j=1,nels
         layer=layr(nel,j)
         atfr(j)=c1(j)/ax(j)
         atfrden(layer)=atfrden(layer)+atfr(j)
30    continue
      do 40 j=1,nels
         layer=layr(nel,j)
         atfr(j)=atfr(j)/atfrden(layer)*100.
40    continue
      do 50 j=1,nels
        call layrelem(ne,nel,layer,j)
        sum(layer,1)=sum(layer,1)+c1(j)
        sum(layer,2)=sum(layer,2)+c1(j)*delta(layer)*1.e6
        
50    continue
      do 60 lu=lu1,lu2
        do 60 j=1,nels
          kt=kr(j)
          call layrelem(ne,nel,layer,j)
          if ((symb(j)(7:7).eq.'S').or.(ifix(layer)).or.
     &      (cfg(3).eq.'K')) kt=krx(j)
          if (layer.lt.7) then
            if ((symb(j)(7:7).eq.'C').or.(symb(j)(7:7).eq.'N')) then
              write(lu,960)layrchar(layer)(1:9),symb(j)(1:2),c1(j)*100.,
     &        atfr(j),c1(j)*delta(layer)*1.e6,
     &        kf(j),kt,ffchar(j),ffcont(j)
            else
              write(lu,950)layrchar(layer)(1:9),symb(j)(1:2),c1(j)*100.,
     &        atfr(j),c1(j)*delta(layer)*1.e6,
     &        kt,ffchar(j),ffcont(j)
            endif
          else
            if ((symb(j)(7:7).eq.'C').or.(symb(j)(7:7).eq.'N')) then
              write(lu,980)layrchar(layer)(1:9),symb(j)(1:2)
     &        ,c1(j)*100.,atfr(j),kf(j),kt,ffchar(j),ffcont(j)
            else
              write(lu,970)layrchar(layer)(1:9),symb(j)(1:2)
     &        ,c1(j)*100.,atfr(j),kt,ffchar(j),ffcont(j)
            endif
          endif
          do 55 kk=1,7
            if ((ne.eq.nel(kk)).and.(layer.eq.kk)) then
              if (kk.lt.7) then
                write(lu,990)sum(kk,1)*100.,100.,sum(kk,2),sum(kk,2)
     &            /rho(kk)*100.
              else if(kk.eq.7) then
                write(lu,991)sum(kk,1)*100,100.
              endif
            endif
55        continue
60    continue
c
910   format(/'                 Composition       Thickness          K-r
     &atio      Fchar Fcont')
920   format ('               Weight%  Atom%  ug/cm**2 Angstrom  Cmp.Std
     & Pure El    %     %')
930   format ('               Weight%  Atom%  ug/cm**2 Angstrom  Cmp.Std
     & Pure El    %     %')
940   format ('               =======  =====  ======== ========  =======
     & =======  ===== =====')
950   format (1x,a9,1x,a2,':',f7.2,1x,f7.2,2x,
     &f8.3,1x,'  ------',2x,'-------',f8.5,1x,2(f6.2))
960   format (1x,a9,1x,a2,':',f7.2,1x,f7.2,2x,
     &f8.3,1x,'  ------',1x,f8.5,f8.5,1x,2(f6.2))
970   format (1x,a9,1x,a2,':',f7.2,1x,f7.2,2x,
     &' -------',1x,'  ------',2x,'-------',f8.5,1x,2(f6.2))
980   format (1x,a9,1x,a2,':',f7.2,1x,f7.2,2x,
     &' -------',1x,'  ------',1x,f8.5,f8.5,1x,2(f6.2))
990   format (' sum         :',f7.2,1x,f7.2,2x,f8.3,1x,f8.1/)
991   format (' sum         :',f7.2,1x,f7.2/)
      goto 9999
1000  continue
      do 1010 lu=lu1,lu2
        write (lu,1910)
        if (cfg(3).eq.'K') write (lu,1920)
        if (cfg(3).eq.'C') write (lu,1930)
        write (lu,1940)
1010    continue
         sum(1,1)=0.
         atfrden(1)=0.
      do 1020 j=1,nels
         atfr(j)=c1(j)/ax(j)
         atfrden(1)=atfrden(1)+atfr(j)
1020  continue
      do 1030 j=1,nels
         atfr(j)=atfr(j)/atfrden(1)*100.
1030  continue
      sum(1,1)=0.
      do 1040 j=1,nels
1040    sum(1,1)=sum(1,1)+c1(j)
      do 1050 lu=lu1,lu2
        do 1050 j=1,nels
          kt=kr(j)
          if ((symb(j)(7:7).eq.'S').or.(symb(j)(7:7).eq.'D')
     &      .or.(cfg(3).eq.'K')) kt=krx(j)
          if (symb(j)(7:7).eq.'C') then
            write (lu,1960)symb(j)(1:2),c1(j)*100.,atfr(j),
     &      kf(j),kt,ffchar(j),ffcont(j)
          else
            write (lu,1950)symb(j)(1:2),c1(j)*100.,atfr(j),
     &      kt,ffchar(j),ffcont(j)
          endif
        if (j.eq.nels) write (lu,1990)sum(1,1)*100.
1050  continue
c
1910  format (/'          Composition        K-ratio        Fchar    Fco
     &nt')
1920  format ('        Weight%  Atom%   Cmp.Std. Pure El.    %        %'
     &)
1930  format ('        Weight%  Atom%   Cmp.Std. Pure El.    %        %'
     &)
1940  format ('        =======  =====   =======  =======   ======   ====
     &=')
1950  format (1x,a2,' :',f9.2,1x,f7.2,2x,' -------',1x,f8.5,1x,2(f8.2))
1960  format (1x,a2,' :',f9.2,1x,f7.2,2x,f8.5,1x,f8.5,1x,2(f8.2))
1970  format(' ')
1990  format (' sum:',f9.2,/)
9999  do 2000 lu=lu1,lu2
2000    write (lu,1970)
      close (unit=5)
      return
      end
