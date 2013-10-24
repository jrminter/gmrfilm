c---------------------------------ELEMENTS------------------------------------
c
c     This program queries operator for the elements' new voltages
c     in a previously analyzed system. Multiple accelerating voltages.
c     Validity of voltage vis-a-vis Ec is checked in subroutine newline
c
c     program completed 5/93 by richard a. waldo
      
      subroutine newvolt(nel,nels,symb,e0,mode)
      real e0(15)
      integer nel(7)
      character*7 symb(15)
      character*5 tempkv
      character*1 mode
      do 300 i=1,nels
50      if (mode.eq.'B') then 
          print 7001,symb(i),e0(i)
        else
          call layrelem(ne,nel,layer,i)
          print 7002,layer,symb(i),e0(i)
        endif
        read 7005,tempkv
        if (tempkv .ne.' ') then
          if (ichar(tempkv(1:1)).gt.58) then
            print 7003,7,7
            print 7004  
            goto 50
          endif
          do 100 j=1,5
            if (tempkv(j:j).eq.'.') goto 200
            if (tempkv(j:j).eq.' ') then
              tempkv(j:j)='.'
              goto 200
            endif
100       continue
200       read(tempkv,fmt='(f5.2)')e0(i)
        else
          e0(i)=15.
        endif
        if (e0(i).le.0.) then
          print 7004
          goto 50
        endif
300   continue

7001  format ('+Enter Eo for element ',a5,' (old=',f4.1,',def=No Change) 
     &:')
7002  format ('+Enter Eo for layer ',i1,' element ',a5,' (old Eo=',f4.1,
     &',def=No Change): ')
7003  format(1x,2a1)
7004  format('+Invalid voltage, try again!',/)
7005  format(a5)      
      return
      end
