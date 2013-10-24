c      
c     get kratios 5/93 r.a. waldo
c
      subroutine getkrat(mode,nel,nels,ifix,st,symb,layrchar,c1,kr,kf
     &,prims,fchars,fconts)
      character*1 mode
      character*10 layrchar(7)
      real c1(15),kr(15),kf(15)
      real prims(15,2),fchars(15,2),fconts(15,2)
      integer nel(7)
      logical ifix(7)
      integer st(15)
      character*7 symb(15)
      character*8 tempkr
      print 995
      do 330 i=1,nels
         layer=layr(nel,i)
         if (ifix(layer)) goto 330
         if (st(layer).eq.i) goto 330
325      if (mode.eq.'F') then
           if ((symb(i)(7:7).eq.'C').or.(symb(i)(7:7).eq.'N')) then
             print 340,'factor',layrchar(layer),symb(i)(1:2),kf(i)
           else
             print 340,'ratio ',layrchar(layer),symb(i)(1:2),kf(i)
           endif
         else
           if ((symb(i)(7:7).eq.'C').or.(symb(i)(7:7).eq.'N')) then
             print 3401,'factor',symb(i)(1:2),kf(i)
           else if (symb(i)(7:7).eq.'D') then
             goto 330
           else
             print 3401,'ratio ',symb(i)(1:2),kf(i)
           endif
         endif
         read 992,tempkr
         if (tempkr.ne.' ') read(tempkr,fmt='(f8.6)')kf(i)
         if (kf(i).le.0.) kf(i)=1.e-9
         kr(i)=kf(i)*(prims(i,2)+fchars(i,2)+fconts(i,2))/
     &               (prims(i,1)+fchars(i,1)+fconts(i,1))
         if ((kr(i).le.0.).or.(kr(i).gt.1.0)) then
           print 993,7,7,7
           print 994
           print 995
           goto 325
         endif
         c1(i)=kr(i)
330   continue

340   format ('+k-',a6,' for ',a10,'element ',a2,' = ',f7.5,' new value
     &: ')
3401  format ('+k-',a6,' for ',a2,' = ',f7.5,' new value : ')
993   format(3a1)
994   format(' Try Again! ')
995   format(' ')
992   format(a8)
      return
      end
