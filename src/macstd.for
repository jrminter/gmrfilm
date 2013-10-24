c------------------------------MACSTD------------------------------
c
c      controls calculation of mass absorption coefficients
c      for compound standards
c      3/88 R.A.W.
c      revised 10/88 for Heinrich's MAC's (ICXOM-11)

      subroutine macstd(z1,z2,at,line1,sy1,sy2,xmu,macchang,caller)
      character*12 tempabs1
      character*2 sy1,sy2
      character*3 lines(12),lb
      character*1 macchang,caller
      data lines/'Kb ','Ka ','Lg2','Lb3','Lb4','Lg1',
     &           'Lb1','Lb2','La1','Mg ','Mb ','Ma '/
      lb=lines(line1)
      call mu(xmu,z1,z2,line1,at)
      if (macchang.ne.'Y') return
      if (caller.eq.'B') print 170,'XRF, Compd. Stnd.: ',sy1,lb,sy2,xmu
      if (caller.eq.'C') print 170,'Compound Standard: ',sy1,lb,sy2,xmu
      if (caller.eq.'P') print 170,'Pure Element Stnd: ',sy1,lb,sy2,xmu
      if (caller.eq.'F') print 170,'Addl. XRF, Sample: ',sy1,lb,sy2,xmu
170   format('+',a19,' mu for ',a2,a3,' in ',a2,' is ',f9.2
     &,' Change to: ')
55    read 9001,tempabs1
      if (tempabs1  .eq.' ') return
      do 56 i=1,12
        if (tempabs1(i:i).eq.'.') goto 57
        if (tempabs1(i:i).eq.' ') then
          tempabs1(i:i)='.'
          goto 57
        endif
56    continue
57    read(tempabs1,fmt='(f10.2)')tempabs2
      xmu=tempabs2
      print 58,xmu
58    format('+Changed to ',f9.2)
      print 9002
      return
9001  format(a12)
9002  format(/)
      end
