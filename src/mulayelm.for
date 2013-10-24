c-------------------------------MULAYELM------------------------------------
c      MULti LAYer ELeMental analysis. Analysis of same element in
c      more than one layer.
c
c      11/90 R.A. Waldo
c
      subroutine mulayelm(symb,icmd7,mle,nels,nel,nelt,mlen,ik)
      integer mle(7),nel(7),nelt(15)
      character*1 icmd7
      character symb(15)*7
c
c Check for analysis of an element present in more than one layer
c
      icmd7='N'
        do 5 i=1,nels
          if ((symb(i)(7:7).eq.'M').or.(symb(i)(7:7).eq.'N')) then
            ik=1
            layer=layr(nel,i)
            mlen=layer
            mle(1)=i
            icmd7='Y'
            do 4 j=1,nels
              if (i.eq.j) goto 4
              if (symb(j)(1:2).eq.symb(i)(1:2)) then
                symb(j)(6:6)=','
                layer=layr(nel,j)
                mle(ik+1)=j
                nelt(ik)=j
                ik=ik+1
              endif
4           continue
          endif
5       continue
      k=ik
      if (k.eq.0) k=1
      do 10 i=1,nels
        do 15 j=2,7
          if (i.eq.mle(j)) goto 10
15      continue
        nelt(k)=i
        k=k+1
10    continue
      return
      end
