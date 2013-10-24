c---------------------------------ELEMENTS------------------------------------
c
c     this program queries operator for the elements
c     and the x-ray lines analyzed.  the program checks that the
c     the elements and x-rays are valid physically.
c
c     program completed 4/87 1987 by richard a. waldo
c     revised 3/88 for analysis by stoichiometry and compound standard, r.a.w.
c     compile with /d switch to include brief instructions on data entry
c      
      subroutine elements(nel,nels,symb,zx,ax,ec,e0,line,stds0,stds1,
     &stds2,mode,standrd,nlayer,voltages)
      real zx(15),ax(15),ec(15),stds1(15,3,15),e0(15)
      integer line(15),nel(7),stds0(15),voltages
      character*7 symb(15)
      character*2 stds2(15,15),help2
      character*5 standrd(15),tempkv
      character*1 mode,at
      ics=0
      if (mode.eq.'F') then
        print 993
10      print 12
12      format ('+Number of layers (count substrate as 1) : ')
        read(5,*,err=10)nlayer
        nels=0
        do 18 kk=1,7
          if (kk.lt.nlayer) then
13          print 14,kk
14          format('+Number of elements in layer ',i1,'       : ')
            read(5,*,err=13)nel(kk)
          else if (kk.lt.7) then
            nel(kk)=0
          else if (kk.eq.7) then
15          print 16
16          format('+Number of elements in the substrate : ')
            read(5,*,err=15)nel(7)
          endif
          nels=nels+nel(kk)
18      continue
        if ((nel(1).eq.0).or.(nel(7).eq.0).or.(nels.gt.15)) goto 10
      else
22      print 23
23      format ('+Number of elements : ')
        read(5,*,err=22)nel(1)
        nels=nel(1)
        if ((nels.eq.0).or.(nels.gt.15)) goto 22
      endif
      print 24
24    format(' Enter elements, x-ray lines, options. (e.g. FeKa,c)')
c      print 25
c25    format(' Type He(lp) for detailed instructions; otherwise hit Ente
c    &r key. ')
c      read 990,help2
c      if ((help2.eq.'He').or.(help2.eq.'he').or.(help2.eq.'HE'))
c    & call helpfile(2)
      do 50 i=1,nels
30      if (mode.eq.'B') then
          if (i.eq.1) print 57,1
          if (i.gt.1) print 58,i
        else
          call layrelem(ne,nel,layer,i)
          if (layer.eq.1) then
            if (i.eq.1) print 59,'layer 1   ',1
            if (i.gt.1) print 60,'layer 1   ',ne
          else if (layer .eq. 7) then
            print 60,'substrate ',ne
          else
            print 61,'layer ',layer,ne
          endif
        endif
34      read 9001,symb(i)
        if (symb(i)(5:5).eq.',') then
          symb(i)(7:7)=symb(i)(6:6)
          symb(i)(6:6)=symb(i)(5:5)
          symb(i)(5:5)=' '
        endif
        help2=symb(i)(1:2)
        if ((help2.eq.'He').or.(help2.eq.'he').or.(help2.eq.'HE')) then
          call helpfile(3)
          goto 30
        endif
        call lookup(symb(i),z,a,ecr,lin)
        if ((z .eq. 93.) .or. (ecr .le. 0.)) then
          print 37
37        format (' Invalid element or x-ray line, try again!',/)
          goto 30
        endif
        if (voltages.gt.1) then
1202      print 1203,symb(i)
1203      format ('+Enter Eo for element ',a5,' (def.=15): ')
          read 991,tempkv
          if (tempkv .ne.' ') then
            if (ichar(tempkv(1:1)).gt.58) then
              print 992,7,7
              print 1204
1204          format('+Invalid voltage, try again!',/)
              goto 1202
            endif
            do 1205 j=1,5
              if (tempkv(j:j).eq.'.') goto 1206
              if (tempkv(j:j).eq.' ') then
                tempkv(j:j)='.'
                goto 1206
              endif
1205        continue
1206        read(tempkv,fmt='(f5.2)')e0(i)
          else
            e0(i)=15.
          endif
          if (e0(i).le.0.) goto 1202
        endif
        if (ecr .gt. e0(i)*0.9) then
          print 40,ecr,e0(i)
40    format (' Overvoltage ratio for ',f5.1,'keV Ec with Eo=',f5.1,'keV
     & is too low! try again.'//)
          goto 30
        endif
        zx(i)=z
        ax(i)=a
        ec(i)=ecr
        line(i)=lin
        if (symb(i)(7:7).eq.'c') symb(i)(7:7)='C'
        if (symb(i)(7:7).eq.'n') symb(i)(7:7)='N'
        at=symb(i)(7:7)
        if ((at.eq.'C').or.(at.eq.'N')) then
          call compstd(stds0,stds1,stds2,i,symb,ics,standrd)
        endif
        if (at.eq.'m') symb(i)(7:7)='M'
        if (at.eq.'d') symb(i)(7:7)='D'
        if (at.eq.'s') symb(i)(7:7)='S'
50    continue
57    format(' Symbol, x-ray for element',i2,' : ')
58    format('+Symbol, x-ray for element',i2,' : ')
59    format(' Symbol, x-ray for ',a10,' element',i2,' : ')
60    format('+Symbol, x-ray for ',a10,' element',i2,' : ')
61    format('+Symbol, x-ray for ',a6,i1,'    element',i2,' : ')
990   format(a2)
991   format(a5)
992   format(2a1)
993   format(/)
9001  format (a7)
      return
      end
