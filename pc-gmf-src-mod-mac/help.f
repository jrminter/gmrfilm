cc     Prints help informationc     5/93 r.a. waldoc      subroutine help      character*1 hlp      hlp=' '      write(*,7001) 7001 format (' Type (H)elp for information on the program, otherwise hi     &t Enter key: ')      read 990,hlp      if ((hlp.eq.'h').or.(hlp.eq.'H')) call helpfile(1)990   format(a1)            return      end