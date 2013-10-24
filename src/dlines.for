      subroutine dlines(iter,delta,icmd3,mode,nels,nlayer,c3)
      real delta(7),c3(15)
      character*1 icmd3,mode
      print 890
      if ((icmd3.eq.'C').and.(mode.eq.'F')) then
        if (iter.eq.0) then
          if (nlayer-1.eq.1) print 891
          if (nlayer-1.eq.2) print 892
          if (nlayer-1.eq.3) print 893
          if (nlayer-1.eq.4) print 894
          if (nlayer-1.eq.5) print 895
          if (nlayer-1.eq.6) print 896
          print 890
        endif
        goto (11,22,33,44,55,66) nlayer-1
11      if (nels.lt.11) then
          print 981,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else
          print 981,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,10)
          print 9811,(c3(ii),ii=11,nels)
        endif
        return
22      if (nels.lt.10) then
          print 982,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else
          print 982,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,9)
          print 9822,(c3(ii),ii=10,nels)
        endif
        return
33      if (nels.lt.9) then
          print 983,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else
          print 983,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,8)
          print 9833,(c3(ii),ii=9,nels)
        endif
        return
44      if (nels.lt.9) then
          print 984,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else
          print 984,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,8)
          print 9844,(c3(ii),ii=9,nels)
        endif
        return
55      if (nels.lt.9) then
          print 985,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else
          print 985,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,8)
          print 9855,(c3(ii),ii=9,nels)
        endif
        return
66      if (nels.lt.7) then
          print 986,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,nels)
        else if (nels.lt.13) then
          print 986,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,6)
          print 9866,(c3(ii),ii=7,nels)
        else
          print 986,iter,(delta(j)*1.e6,j=1,nlayer-1),(c3(ii),ii=1,6)
          print 9866,(c3(ii),ii=7,12)
          print 9866,(c3(ii),ii=13,nels)
        endif
        return
      else if ((icmd3.eq.'C').and.(mode.eq.'B')) then
        if (iter.lt.0) then
          print 897
          print 890
        endif
        if (nels.lt.12) then
          print 987,(c3(ii),ii=1,nels)
        else
          print 987,(c3(ii),ii=1,11)
          print 9877,(c3(ii),ii=12,nels)
        endif
        return
      endif
890   format(' ')
891   format('+','iter  d1  |--Weight Fractions-->')
892   format('+','iter  d1     d2  |--Weight Fractions-->')
893   format('+','iter  d1     d2     d3  |--Weight Fractions-->')
894   format('+','iter d1    d2    d3    d4  |--Weight Fractions-->')
895   format('+','iter d1    d2    d3    d4   d5  |--Weight Fractions-->
     &')
896   format('+','iter d1    d2    d3    d4    d5    d6  |--Weight Fract
     &ions-->')
897   format('+ |--Weight Fractions-->')
981   format('+',i2,1x,f6.1,10(1x,f6.4):)
9811  format(10x,4(1x,f6.4):)
982   format('+',i2,2(1x,f6.1),9(1x,f6.4):)
9822  format(17x,5(1x,f6.4):)
983   format('+',i2,3(1x,f6.1),8(1x,f6.4):)
9833  format(24x,6(1x,f6.4):)
984   format('+',i2,4(1x,f5.0),8(1x,f5.3):)
9844  format(27x,7(1x,f6.4):)
985   format('+',i2,4(1x,f5.0),1x,f4.0,8(1x,f5.3):)
9855  format(32x,7(1x,f5.3):)
986   format('+',i2,6(1x,f5.0),6(1x,f5.3):)
9866  format(39x,6(1x,f5.3):)
987   format('+',1x,11(1x,f6.4):)
9877  format(2x,4(1x,f6.4):)
      end
