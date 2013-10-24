c      
c     get take-off angle 5/93 r.a. waldo
c
      subroutine gettoa(toa)
      character*5 temptoa
50    print 60
60    format(' Enter take-off angle (default=40 degrees) : ')
      read 991,temptoa
      if (temptoa .ne.' ') then
        do 70 i=1,5
          if (temptoa(i:i).eq.'.') goto 80
          if (temptoa(i:i).eq.' ') then
            temptoa(i:i)='.'
            goto 80
          endif
70      continue
80      read(temptoa,fmt='(f5.2)')toa
        else
          toa=40.
        endif
      if ((toa.gt.90.).or.(toa.le.0.)) goto 50
991   format(a5)      
      return
      end
