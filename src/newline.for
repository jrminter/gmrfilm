c---------------------------------NEWLINE------------------------------------
c
c     This program queries operator for a new line for an element
c     because the old line was not excited at the new operating potential.
c
c     program completed 5/91 by richard a. waldo
c
      subroutine newline(nels,symb,e0,ec,line)
      real ec(15),e0(15)
      integer line(15)
      character*7 symb(15)
      do 100 i=1,nels
        j=0
20      j=j+1
        call lookup(symb(i),z,a,ecr,lin)
        if ((ecr.le.e0(i)*0.9).and.(ecr.gt.0.)) then
          goto 99
        else if (ecr .gt. e0(i)*0.9) then
          print 40,symb(i)(1:5),ecr,e0(i)
40    format (' Overvoltage ratio for ',a5,'(',f5.1,' keV Ec) with Eo='
     &,f5.1,'keV is too low!',/,' Enter another line (e.g. La1, Ka): ')
        read 990,symb(i)(3:5)
        if (j.eq.4) then
          stop 'You did not enter a valid line in four tries.'
        else
          goto 20
        endif
        else if (ecr.le.0.) then
          print 50
50        format(' Invalid line for this element; try again: ')
          read 990,symb(i)(3:5)
        if (j.eq.4) then
          stop 'You did not enter a valid line in four tries.'
        else
          goto 20
        endif
        endif
99      ec(i)=ecr
        line(i)=lin
100   continue
990   format(a3)
      return
      end
