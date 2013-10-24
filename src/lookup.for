c-------------------------------LOOKUP--------------------------------
c
c     This program find the atomic weight and number for an element.
c     The data input has the form feka, NdLb3,n, etc. for example.
c     Program completed 4/87 by richard a. waldo
c     Revised 3/91, r.a.w.
c
      subroutine lookup(symbol,z,a,ecr,line)
      real axtab(92)
      character*7 symbol
      character*6 zsymb(92)
      data zsymb/'H H h ','HEHehe','LILili','BEBebe','B B b ','C C c ',
     &'N N n ','O O o ','F F f ','NENene','NANana','MGMgmg','ALAlal',
     &'SISisi','P P p ','S S s ','CLClcl','ARArar','K K k ','CACaca',
     &'SCScsc','TITiti','V V v ','CRCrcr','MNMnmn','FEFefe','COCoco',
     &'NINini','CUCucu','ZNZnzn','GAGaga','GEGege','ASAsas','SESese',
     &'BRBrbr','KRKrkr','RBRbrb','SRSrsr','Y Y y ','ZRZrzr','NBNbnb',
     &'MOMomo','TCTctc','RURuru','RHRhrh','PDPdpd','AGAgag','CDCdcd',
     &'INInin','SNSnsn','SBSbsb','TETete','I I i ','XEXexe','CSCscs',
     &'BABaba','LALala','CECece','PRPrpr','NDNdnd','PMPmpm','SMSmsm',
     &'EUEueu','GDGdgd','TBTbtb','DYDydy','HOHoho','ERErer','TMTmtm',
     &'YBYbyb','LULulu','HFHfhf','TATata','W W w ','RERere','OSOsos',
     &'IRIrir','PTPtpt','AUAuau','HGHghg','TLTltl','PBPbpb','BIBibi',
     &'POPopo','ATAtat','RNRnrn','FRFrfr','RARara','ACAcac','THThth',
     &'PAPapa','U U u '/
c
c    axtab contains atomic weight for each element
c
      data axtab/
     &  1.008,  4.003,  6.939,  9.012, 10.81 , 12.011, 14.007,
     & 15.999, 18.998, 20.183, 22.99 , 24.312, 26.982, 28.086,
     & 30.974, 32.064, 35.453, 39.948, 39.102, 40.08 , 44.96 ,
     & 47.9  , 50.942, 51.996, 54.938, 55.847, 58.933, 58.71 ,
     & 63.54 , 65.37 , 69.72 , 72.59 , 74.922, 78.96 , 79.909,
     & 83.8  , 85.47 , 87.62 , 88.905, 91.22 , 92.91 , 95.94 ,
     & 99.   ,101.07 ,102.90 ,106.4  ,107.87 ,112.4  ,114.82 ,
     &118.69 ,121.75 ,127.6  ,126.904,131.3  ,132.905,137.34 ,
     &138.91 ,140.12 ,140.91 ,144.24 ,147.   ,150.35 ,151.96 ,
     &157.25 ,158.924,162.5  ,164.93 ,167.26 ,168.93 ,173.04 ,
     &174.97 ,178.49 ,180.95 ,183.85 ,186.2  ,190.2  ,192.2  ,
     &195.09 ,196.97 ,200.59 ,204.37 ,207.19 ,208.98 ,210.   ,
     &210.   ,222.   ,223.   ,226.   ,227.   ,232.04 ,231.   ,
     &238.03 /
c
      do 90 j=1,92
      if ((symbol(1:2) .eq. zsymb(j)(1:2)).or.
     &    (symbol(1:2) .eq. zsymb(j)(3:4)).or.
     &    (symbol(1:2) .eq. zsymb(j)(5:6))) goto 95
90    continue
95    z=j*1.
      a=axtab(j)
      symbol(1:2)=zsymb(j)(3:4)
      if (symbol(3:3) .eq. 'c') return
      if (symbol(4:4).eq.'A') symbol(4:4)='a'
      if (symbol(4:4).eq.'B') symbol(4:4)='b'
      if (symbol(4:4).eq.'G') symbol(4:4)='g'
      if (symbol(3:3) .eq. 'k') symbol(3:3)='K'
      if (symbol(3:3) .eq. 'l') symbol(3:3)='L'
      if (symbol(3:3) .eq. 'm') symbol(3:3)='M'
      if (symbol(3:5).eq.'Lg3') symbol(3:5)='Lg2'
      if (symbol(3:5).eq.'Lb ') symbol(3:5)='Lb1'
      if (symbol(3:5).eq.'La ') symbol(3:5)='La1'
      if ((symbol(3:4).ne.'Ka').and.(symbol(3:4).ne.'Kb').and.
     &    (symbol(3:5).ne.'La1').and.(symbol(3:5).ne.'Lb1').and.
     &    (symbol(3:5).ne.'Lb2').and.(symbol(3:5).ne.'Lb3').and.
     &    (symbol(3:5).ne.'Lb4').and.(symbol(3:5).ne.'Lg1').and.
     &    (symbol(3:5).ne.'Lg2').and.(symbol(3:4).ne.'Mg').and.
     &    (symbol(3:4).ne.'Mb').and.(symbol(3:4).ne.'Ma')) then
c
c     if no match return error flag of -1.
c     note: if E(crit) returned has value of 0.0,
c     this also serves as error flag
c
         ecr=-1.
         return
      endif
100   continue
      ecr=edge(j,symbol(3:5))
      if (symbol(3:4).eq.'Ka') line=2
      if (symbol(3:4).eq.'Kb') line=1
      if (symbol(3:5).eq.'La1') line=9
      if (symbol(3:5).eq.'Lb1') line=7
      if (symbol(3:5).eq.'Lb2') line=8
      if (symbol(3:5).eq.'Lb3') line=4
      if (symbol(3:5).eq.'Lb4') line=5
      if (symbol(3:5).eq.'Lg1') line=6
      if (symbol(3:5).eq.'Lg2') line=3
      if (symbol(3:4).eq.'Mg')  line=10
      if (symbol(3:4).eq.'Mb')  line=11
      if (symbol(3:4).eq.'Ma')  line=12
      return
      end
