c------------------------------STOICH0----------------------------------
c
c      input of valences when one element is
c      analyzed by stoichiometry
c      completed 3/88 by r.a.waldo
c
       subroutine stoich0(ifix,nel,nels,st,symb,val)
       integer st(7),val(15),nel(7)
       logical ifix(7)
       character*7 symb(15)
       logical linefeed
       linefeed=.false.
       do 40 i=1,nels
         if ((symb(i)(7:7).eq.'s').or.(symb(i)(7:7).eq.'S')) then
           print 30
30         format(' Enter valences as positive integers;',/,
     &' calculations automatically assume last valence entered',/,
     &' has the opposite sign of the the first element(s).'/)
           goto 45
         endif
40     continue
45     continue
       do 50 i=1,nels
         layer=layr(nel,i)
         if ((symb(i)(7:7).eq.'s').or.(symb(i)(7:7).eq.'S')) then
           st(layer)=i
         else
           st(layer)=0
         endif
50     continue
       do 100 i=1,nels
         layer=layr(nel,i)
         if (ifix(layer)) goto 100
         if (st(layer).lt.1) goto 100
         if (.not.linefeed) print 990
         linefeed=.true.
         if (layer.lt.7) print 10,layer,symb(i)(1:2)
10       format('+Enter valence for layer',i2,' element ',a2,': ')
         if (layer.eq.7) print 11,symb(i)(1:2)
11       format('+Enter valence for substrate element ',a2,': ')
         read *,val(i)
20       format(i1)
100    continue
990    format(' ')
       return
       end
