c      
c     Returns unique filename based on time and date of opening
c     Also asks if output should also be directed to a printer
c     5/93 r.a. waldo
c
      subroutine getfname(finame,printer)
      character*12 finame
c JRM - just write to grmfout.txt
c
c      character*1 printer
c      character*2 cy1,cmo1,cd1,cm1
c      character*1 ch1
c      call gettim(ih,im,is,ihs)
c      call getdat(iy,imo,id)
c      iy2=mod(iy,100)
c      write(cy1,fmt='(i2)')iy2
c      write(cmo1,fmt='(i2)')imo
c      write(cd1,fmt='(i2)')id
c      write(cm1,fmt='(i2)')im
c      ih2=64+ih
c      write(ch1,fmt='(a1)')ih2
c      finame='FI'//cy1//cmo1//cd1//'.'//ch1//cm1
c      if (iy2.lt.10) finame(3:3)='0'
c      if (imo.lt.10) finame(5:5)='0'
c      if (id.lt.10) finame(7:7)='0'
c      if (im.lt.10) finame(11:11)='0'
      finame='grmfout.txt'
c      printer='S'
c     write(*,7002)finame
c7002 format(' Results are automatically saved to ascii file "',a12,'";'
c    &,/,' (i.e. to file FIyymmdd.hmm where yy-dd-mm hmm is the current'
c    &,/,' date and time; h is the hour from A-X on a 24-hour clock.'
c    &,//,' Direct output to Printer and CRT (P), or CRT only (S) Def.=S
c    &) ? ')
c     read 990,printer
c      if ((printer.eq.' ').or.(printer.eq.'s')) printer='S'
c      if (printer.eq.'p') printer='P'
c 990   format(a1)
      return
      end
