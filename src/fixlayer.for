c      
c     Asks if a layer composition and/or thickness is preset (fixed) to
c     a known value. 5/93 r.a. waldo
c
      subroutine fixlayer(mode,cfg,nel,nels,delta
     &,symb,c1,layrchar,ifix,rho)
      character*1 mode,reply
      character*1 cfg(10)
      character*7 symb(15),tsymb(15)
      character*9 label
      character*10 layrchar(7)
      real delta(7),rho(7)
      real c1(15),c4(15)
      real form(15)
      integer nel(7),nl(7)
      logical ifix(7)
      if(cfg(1).eq.'Y') goto 310
      if(cfg(2).eq.'Y') goto 170
      if (cfg(10).ne.'N') goto 310
      if((mode.eq.'B').and.(cfg(3).eq.'C')) then
        ifix(1)=.false.
        return
      endif
      do 10 jj=1,7
        ifix(layer)=.false.
10    continue
      n=7
      if (cfg(3) .eq. 'K') then
        if (mode.eq.'B') then
          n=1
          ifix(1)=.true.
          goto 211
        endif
        do 137 i=1,7
          ifix(i)=.true.
          if (nel(i).eq.0) ifix(i)=.false.
137     continue
        goto 170
      endif
      print 991
      do 160 i=1,7
        if (nel(i).eq.0) goto 160
        if (i.lt.7) print 140,i
140     format ('+Fix composition and thickness of layer ',i1,'? (n): ')
        if (i.eq.7) print 150
150     format ('+Fix composition of substrate? (n) : ')
        read 990,reply
        if ((reply.eq.'y').or.(reply.eq.'Y')) ifix(i)=.true.
160   continue
170   if (mode.eq.'B') goto 211
      icount=1
      do 210 i=1,6
        if (.not.ifix(i)) goto 210
174     if (icount.eq.1) then
          print 175
175     format('+Enter fixed thicknesses in Angstroms (a) or ug/cm**2 (u
     &), (def=a): ')
          read 990,cfg(5)
        endif
        if ((cfg(5).eq.' ').or.(cfg(5).eq.'a')) cfg(5)='A'
        if ((cfg(5).ne.'A').and.(cfg(5).ne.'u').and.(cfg(5).ne.'U'))
     &    goto 174
        if (cfg(5).eq.'A') label='Angstroms'
        if ((cfg(5).eq.'u').or.(cfg(5).eq.'U')) label=' ug/cm**2'
180     print 190,i,label
190     format ('+Layer ',i1,' fixed thickness (',a9,'): ')
        read (5,*,err=180)delta(i)
        if (delta(i).le.0.) then
          print 200
200       format (' !thickness out of range! try again',/)
          goto 180
        endif
        delta(i)=delta(i)/1.e6
        if (cfg(5).eq.'A') delta(i)=delta(i)*rho(i)/100.
        icount=2
210   continue
211   do 300 i=1,n
212     if (.not.ifix(i)) goto 300
        if (mode.eq.'F') then
          if (nel(i).eq.1) then
            do 213 j=1,nels
              layer=layr(nel,j)
              if ((i.eq.layer).and.(ifix(layer))) c1(j)=1.
213         continue
            goto 300
          else
            if (i.lt.7) then
            print 215,i
215         format(' Enter the fixed composition of layer ',i1,' by '
     &      ,/,' weight fractions (w) or by atomic formula (def=a) : ')
            else
            print 217
217         format(' Enter the fixed composition of the substrate by '
     &      ,/,' weight fractions (w) or by atomic formula (def=a) : ')
            endif
          endif
        endif
        if (mode.eq.'B') print 220
220     format(' Enter the fixed composition by '
     &  ,/,' weight fractions (w) or by atomic formula (def=a) : ')
        read 990,reply
        if ((reply.eq.' ').or.(reply.eq.'a')) reply='A'
        if ((reply.eq.'W').or.(reply.eq.'w')) reply='W'
        if ((reply.ne.'W').and.(reply.ne.'A')) goto 212
        if (reply.eq.'W') then
          do 250 j=1,nels
            layer=layr(nel,j)
            if ((i.eq.layer).and.(ifix(layer))) then
230           if (mode.eq.'F') print 240,layrchar(i),symb(j)(1:2)
              if (mode.eq.'B') print 2401,symb(j)(1:2)
              read (5,*,err=230)c1(j)
              if (c1(j) .lt. 0.) goto 230
              if (c1(j).eq.0.) c1(j)=1.e-9
240           format ('+Weight fraction concentration of ',a10,' element
     & ',a2,' : ')
2401          format ('+Weight fraction concentration of ',a2,' : ')
            endif
250       continue
        else
          lf=0
          nl(i)=nel(i)
          do 280 j=1,nels
            layer=layr(nel,j)
            if ((i.eq.layer).and.(ifix(layer))) then
              lf=lf+1
              tsymb(lf)(1:2)=symb(j)(1:2)
              tsymb(lf)(3:3)='c'
260           if (mode.eq.'F') print 270,layrchar(i),symb(j)(1:2)
              if (mode.eq.'B') print 2701,symb(j)(1:2)
              read (5,*,err=260)form(lf)
              if (form(lf) .lt. 0.) goto 260
              if (form(lf).eq.0.) form(lf)=1.e-9
270           format ('+Atomic formula of ',a10,' element ',a2,' : ')
2701          format ('+Atomic formula of ',a2,' : ')
            endif
280       continue
          call wtfract(tsymb,form,nl,c4)
          nl(i)=0
          lf=0
          do 290 j=1,nels
            layer=layr(nel,j)
            if ((i.eq.layer).and.(ifix(layer))) then
              lf=lf+1
              c1(j)=c4(lf)
            endif
290       continue
        endif
300   continue
310   do 320 i=1,2
        if (.not.ifix(i)) delta(i)=0.
320   continue
      do 340 i=1,nels
         layer=layr(nel,i)
         if ((ifix(layer)).and.(cfg(3).eq.'C')) symb(i)(6:6)='!'
340   continue
990   format(a1)
991   format(' ')
      return
      end


