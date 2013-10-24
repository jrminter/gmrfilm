c----------------------------COMPSTD-------------------------------
c
c    controls input of compound standard physical data
c    3/88 r.a. waldo
c

      subroutine compstd(stds0,stds1,stds2,istd,symb,ics,standrd)
      real stds(15),stds1(15,3,15),formstd(15),wtfrel(15)
      integer stds0(15),n(7)
      character*7 symb(15),symbol,tsymb(15)
      character*2 stds2(15,15),el(15)
      character*1 icmd,icmd5,null
      character*5 standrd(15),stdname
      null=' '
      open (unit=2,file='standard.dat',status='unknown')
        ics=ics+1
        print 80
80      format('+Enter standard name (five characters max.) : ')
        read 82,standrd(ics)
82      format(a5)
        if (ics.gt.1) then
          do 95 k=1,ics-1
            if ((standrd(ics)(1:5).eq.standrd(k)(1:5))) then
              stds0(istd)=k
              ics=ics-1
              print 995
              goto 999
            endif
95        continue
        endif

99      read (2,100,end=183)stdname,nel
100     format(a5,1x,i2)
        if (standrd(ics).eq.stdname) then
          stds0(istd)=ics
          stds2(1,ics)=symb(istd)(1:2)
          print 115
115       format('+Element   Wt. Fraction')
          read (2,117)(el(i),wtfrel(i),i=1,nel)
117       format(15(a2,f8.5,3x):)
          do 130 i=1,nel
            print 120,el(i),wtfrel(i)
120         format(3x,a2,9x,f7.5)
130       continue
140       print 141
141       format(' This standard (y)? : ')
          read 143,icmd
143       format(a1)
          print*,' '
          if (icmd.eq.' ') icmd='y'
          if ((icmd.ne.'y').and.(icmd.ne.'Y').and.
     &        (icmd.ne.'n').and.(icmd.ne.'N')) goto 140
          if ((icmd.eq.'Y').or.(icmd.eq.'y')) then
            symbol(1:2)=symb(istd)(1:2)
            symbol(3:3)='c'
            call lookup(symbol,z,a,e,l)
            stds1(1,2,ics)=z
            stds1(1,3,ics)=a
            do 150 i=1,nel
              if (el(i).eq.symb(istd)(1:2)) stds1(1,1,ics)=wtfrel(i)
150         continue
            nn=1
            do 160 j=1,nel
              if (el(j).eq.symb(istd)(1:2)) goto 160
              nn=nn+1
              symbol(1:2)=el(j)
              symbol(3:3)='c'
              call lookup(symbol,z,a,e,l)
              stds2(nn,ics)=symbol(1:2)
              stds1(nn,1,ics)=wtfrel(j)
              stds1(nn,2,ics)=z
              stds1(nn,3,ics)=a
160         continue
            goto 999
          endif
        else
          read (2,182)el(1)
182       format(a2)
          goto 99
        endif
183     if (ics.gt.1) then
          do 185 k=1,ics-1
            if (standrd(ics)(1:5).eq.standrd(k)(1:5)) then
              stds0(istd)=k
              ics=ics-1
              rewind (2)
              close (2,status='keep')
              return
            endif
185       continue
        endif
        print 196
196     format('+Input standard composition in wt.% (w) or atomic formul
     &a (def=a) : ')
        read 197,icmd5
        if (icmd5.eq.' ') icmd5='a'
197     format(a1)
        stds0(istd)=ics
        stds2(1,ics)=symb(istd)(1:2)
        if ((icmd5.eq.'w').or.(icmd5.eq.'W')) then
1971      print 1981,symb(istd)(1:2),standrd(ics)(1:5)
          read(5,*,err=1971)stds1(1,1,ics)
        else
1972      print 1982,symb(istd)(1:2),standrd(ics)(1:5)
          read(5,*,err=1972)formstd(1)
        endif
1981    format('+Weight fraction of ',a2,' in ',a5,' : ')
1982    format('+Atomic formula of ',a2,' in ',a5,'  : ')
        symbol(1:2)=symb(istd)(1:2)
        symbol(3:3)='c'
        call lookup(symbol,z,a,e,l)
        stds1(1,2,ics)=z
        stds1(1,3,ics)=a
        print 200
200     format('+Number of companion elements (max=14): ')
        read 201,ice
201     format(i1)
        do 230 j=2,ice+1
          print 215,j-1
215       format('+Symbol of companion element #',i1,' : ')
          read 216,stds2(j,ics)
216       format(a2)
          symbol(1:2)=stds2(j,ics)
          symbol(3:3)='c'
          call lookup(symbol,z,a,e,l)
          stds2(j,ics)=symbol(1:2)
          if ((icmd5.eq.'w').or.(icmd5.eq.'W')) then
2170        print 1981,stds2(j,ics),standrd(ics)(1:5)
          read(5,*,err=2170)stds1(j,1,ics)
          else
2172        print 1982,stds2(j,ics),standrd(ics)(1:5)
            read(5,*,err=2172)formstd(j)
            tsymb(j)(1:2)=stds2(j,ics)
            tsymb(j)(3:3)='c'
          endif
          stds1(j,2,ics)=z
          stds1(j,3,ics)=a
230   continue
          if ((icmd5.eq.'a').or.(icmd5.eq.'A')) then
            tsymb(1)(1:2)=stds2(1,ics)
            tsymb(1)(3:3)='c'
            n(1)=ice+1
            call wtfract(tsymb,formstd,n,stds)
            do 240 m=1,ice+1
              stds1(m,1,ics)=stds(m)
240         continue
          endif
      backspace 2
      backspace 2
      backspace 2
      print 990,standrd(ics)
      print 991,(stds2(i,ics),stds1(i,1,ics),i=1,ice+1)
      print 995
      write (2,992)standrd(ics),ice+1
      write (2,993)(stds2(i,ics),stds1(i,1,ics),i=1,ice+1)
      write (2,994)null
      write (2,994)null
990   format(1x,a5,' compound standard composition, weight fractions:')
991   format(5x,15(a2,f8.5,3x):/)
992   format(a5,1x,i2)
993   format(15(a2,f8.5,3x):)
994   format(a1)
995   format(' ')      
      rewind (2)
999   close (2,status='keep')
      return
      end
