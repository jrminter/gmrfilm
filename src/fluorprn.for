c-------------------------------FLUORPRN------------------------------------
c
c
c Printout of x-ray intensity data
c 4/91 R. A. Waldo
c
      subroutine fluorprn(symb,nel,nels,stds0,standrd,prim,prims,
     &fcont,fconts,fchar,fchars,ianalys)
      real prim(15),prims(15,2),fcont(15),fchar(15),fconts(15,2),
     &fchars(15,2)
      integer nel(7),stds0(15)
      character*7 symb(15)
      character*5 standrd(15),std
      idum=ianalys
c       if (ianalys.eq.1) print 100
c100   format(
c     &' Fchar and Fcont: characteristic x-ray and continuum x-ray',/,
c     &' fluorescence factors, respectively.  Defined as: (kratio',/,
c     &' calculated with char.(cont.) fluorescence contributions minus',/
c     &,
c     &' kratio calculated without char.(cont.) fluorescence contribution
c     &s)',/,
c     &' divided by the kratio calculated with char.(cont.) fluorescence'
c     &,/,
c     &' contributions.  This can be understood as the theoretical % erro
c     &r',/,
c     &' added to the analysis if characteristic (or continuum) x-ray',/,
c     &' fluorescence contributions are ignored.  If a compound standard'
c     &,/,
c     &' is used for the element, the factors Fchar and Fcont are',/,
c     &' calculated with respect to the compound standard k-factor.',/,
c     &' A negative factor indicates stronger fluorescence in the standar
c     &d;',/,
c     &' a positive factor indicates stronger fluorescence in the specime
c     &n.')
       print 990,' '
       print 972
972   format(8x,'Emitted Standard X-ray Intensity/Electron (x10^6)',/,
     &' =============================================================='
     &,/,
     &' Strd.  El     Ip       If      Ic     I(tot)   If/Ip,%  Ic/Ip,%'
     &,/,
     &' =====  ==  ========  ======  ======  ========  =======  ======='
     &)
      do 980 i=1,nels
        if (symb(i)(7:7).eq.'C') then
          do 973 ijk=1,i-1
            if ((stds0(i).eq.stds0(ijk)).and.
     &      (symb(i)(1:2).eq.symb(ijk)(1:2))) goto 974
973       continue
c          if (standrd(stds0(i))(5:5).eq.' ') then
c            std(1:5)=''//standrd(stds0(i))(1:4)
c          else
            std=standrd(stds0(i))
c          endif
          print 981,std,symb(i)(1:2),prims(i,2)*1.e6,
     &    fchars(i,2)*1.e6,fconts(i,2)*1.e6,(prims(i,2)+
     &    fchars(i,2)+fconts(i,2))*1.e6,fchars(i,2)/
     &    prims(i,2)*100.,fconts(i,2)/prims(i,2)*100.
974     continue
      endif
      do 975 ijk=1,i-1
        if ((stds0(ijk).eq.0).and.
     &    (symb(i)(1:5).eq.symb(ijk)(1:5))) goto 976
975   continue
      print 981,'P.E. ',symb(i)(1:2),prims(i,1)*1.e6,
     &  fchars(i,1)*1.e6,fconts(i,1)*1.e6,(prims(i,1)+fchars(i,1)+
     &  fconts(i,1))*1.e6,fchars(i,1)/prims(i,1)*100.,fconts(i,1)/
     &  prims(i,1)*100.
976   continue
980   continue
981   format(1x,a5,2x,a2,2x,f8.2,2(2x,f6.2),2x,f8.2,3x,f5.2,4x,f5.2)
      print 990,' '
      print 982
982   format(8x,'Emitted Specimen X-ray Intensity/Electron (x10^6)',/,
     &' =============================================================='
     &,/,
     &' Layer  El     Ip       If      Ic     I(tot)   If/Ip,%  Ic/Ip,%'
     &,/,
     &' =====  ==  ========  ======  ======  ========  =======  ======='
     &)
      do 985 i=1,nels
        jk=layr(nel,i)
        if (jk.eq.7) then
          print 981,'Subs.',symb(i)(1:2),prim(i)*1.e6,fchar(i)
     &    *1.e6,fcont(i)*1.e6,(prim(i)+fchar(i)+fcont(i))*1.e6,
     &    fchar(i)/prim(i)*100.,fcont(i)/prim(i)*100.
        else
          print 984,jk,symb(i)(1:2),prim(i)*1.e6,fchar(i)*1.e6,
     &      fcont(i)*1.e6,(prim(i)+fchar(i)+fcont(i))*1.e6,
     &      fchar(i)/prim(i)*100.,fcont(i)/prim(i)*100.
984       format(3x,i1,4x,a2,2x,f8.2,2(2x,f6.2),2x,f8.2,3x,f5.2,
     &    4x,f5.2)
         endif
985   continue
      print 990,' '
      print 990,' '
990   format(a1)
      return
      end
