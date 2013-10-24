c------------------------------absionrt--------------------------------
c RaTio of IONizations to total ABSorptions
c 3/91 r.a. waldo
c
      function absionrt(za,jedge,xenergy)
      real l1,l2,l3,jr1,jr2,jr3
      nza=int(za)
      goto (1,2,2,2,1,1,1,1,1,3,3,3)jedge
1     absionrt=(rjump(za,jedge)-1.)/rjump(za,jedge)
      return
2     jr1=rjump(za,2)
      jr2=rjump(za,3)
      jr3=rjump(za,4)
      l1=edge(nza,'Lb3')
      l2=edge(nza,'Lb1')
      l3=edge(nza,'La1')
      if (jedge.eq.2) then
        absionrt=(jr1-1.)/jr1
      else if (jedge.eq.3) then
        if ((xenergy.lt.l1).and.(xenergy.gt.l2)) then
          absionrt=(jr2-1.)/jr2
        else if (xenergy.gt.l1) then
          absionrt=(jr2-1.)/jr1/jr2
        endif
      else if (jedge.eq.4) then
        if ((xenergy.lt.l2).and.(xenergy.gt.l3)) then
          absionrt=(jr3-1.)/jr3
        else if ((xenergy.lt.l1).and.(xenergy.gt.l2)) then
          absionrt=(jr3-1.)/jr2/jr3
        else if (xenergy.gt.l1) then
          absionrt=(jr3-1.)/jr1/jr2/jr3
        endif
      endif
      return
3     absionrt=1.
      return
      end