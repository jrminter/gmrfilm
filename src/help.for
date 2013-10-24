c
c     Prints help information
c     5/93 r.a. waldo
c
      subroutine help
      character*1 hlp
      hlp=' '
c     write(*,7001)
c7001 format (' Type (H)elp for information on the program, otherwise hi
c    &t Enter key: ')
c     read 990,hlp
      if ((hlp.eq.'h').or.(hlp.eq.'H')) call helpfile(1)
990   format(a1)      
      return
      end
