c-------------------------------GMRFILM------------------------------------
c
c This program solves the thin film problem of a surface film on a buried
c layer on a substrate.  This is the main program which controls data input
c (by calling of subprograms) and calculation (by method of successive
c approximations) of the compositions and film thicknesses of each layer.
c r.a. waldo 4/87
c
c   To include help information during running of program, 
c   compile subroutines 'getfname', 'elements', and 'help'
c   with /d switch 
c
c 03/88: Analysis by stoichiometry and compound standard, r.a.w.
c 06/88: Characteristic x-ray fluorescence correction, r.a.w.
c 10/88: Bastin's NBS workshop model (1988), r.a.w.
c        (NOTE: see Scanning, Vol. 12, 1990, p.225 for this model)
c 10/88: Heinrich's ICXOM-11 Mass Absorp. Coefficient Equations, r.a.w.
c 09/89: Combined thin film/bulk analysis, r.a.w.
c 10/89: Pouchou and Pichoir (PAP) model, r.a.w.
c 01/90: Improved data output, r.a.w.
c 10/90: Analysis of an element present in more than one layer, r.a.w.
c 11/90: Analysis of up to 7 layers, r.a.w.
c 02/91: Rigorous characteristic x-ray fluorescence correction, r.a.w.
c 04/91: Continuum x-ray fluorescence correction, r.a.w.
c 05/93: Analysis of elements at more than one beam voltage, r.a.w.
c
      real zx(15),ax(15),mac(15,15),kr(15),kf(15),krx(15),ec(15),e0(15),
     &c1(15),c2(15),c3(15),c4(15),chi(7),chiovl(6),delta(7),rho(7),
     &sc(7),za(2),exciter(200,3),fmac(15,200),fcont(15),fchar(15),
     &prim(15),fconts(15,2),fchars(15,2),prims(15,2),cstd(15),
     &stds1(15,3,15)
      integer line(15),st(7),nel(7),stds0(15),mle(7),nelt(15),val(15)
      integer voltages 
      logical notcnvrg,ifix(7),solution
      character symb(15)*7,layrchar(7)*10,standrd(15)*5
      character*1 phipar,cfg(10),mode,macchang
      character*12 finame
      character*2 stds2(15,15)
      data layrchar/'layer 1   ','layer 2   ','layer 3   ','layer 4   ',
     &              'layer 5   ','layer 6   ','substrate '/
c
c
c                          Configuration Array
c =========================================================================
c Cfg(1)   Y=More k-ratios             N=No more k-ratios
c
c Cfg(2)   Y=Calculate more k-ratios   N=No more calculations
c            from compositions
c Cfg(3)   C=Calculate compositions    K=Calculate k-ratios from
c            from k-ratios               compositions and thicknesses
c
c Cfg(4)   Y=New system (specimen)     N=Same specimen   
c
c Cfg(5)   Y=One accelerating voltage  N=More than one acc. voltage    
c
c Cfg(6)   S=Results to CRT            P=Results to CRT and printer
c
c Cfg(7)   Y=Analysis of element       N=No solution requested for specimen
c            present in >1 layer         with element present in > 1 layer
c
c Cfg(8)   Y=Include continuum corr.   N=Do not include continuum correction
c
c Cfg(9)   Y=Print x-ray intensities   N=Do not print
c
c Cfg(10)  N=No change in configuration for next analysis (initial setting)   
c          E=Change acceleration voltage(s), Eo, for next calculation
c          M=  "       Phi(rz) model              "   "       "
c          B=  "     both Eo and model            "   "       "
c          C=  Either include or remove continuum correction only.

c     Global initialization, a new specimen.
      cfg(10)='N'
1     call init0(ifix)

c     Initialize some more variables, same specimen, different conditions.  
2     call init1(ianalys,cfg)

c     Initialize some variables, another analysis, same specimen.
3     call init2(iter,solution,fcont,fchar,prim)

c     Same specimen configuration, more k-ratios or calculations.  
      if ((cfg(1) .eq. 'Y').or.(cfg(2) .eq. 'Y')) goto 100
      
      if (cfg(4) .ne. 'Y') then
        call help
        call getfname(finame,cfg(6))
      endif
      
      if (cfg(10).eq.'N') then

c       Initialize some more variables.
        call init3(symb,stds0,stds1,stds2,nel,delta)

c       Get analysis configuration switches.
        call getcfg(cfg,phipar,mode,macchang)

c       Get take-off angle.      
        call gettoa(toa)
      
      endif
      
c     Initialize some more variables.
      call init4(fconts,fchars,prims)      
      
      if ((cfg(10).eq.'M').or.(cfg(10).eq.'B').or.(cfg(10).eq.'N')) 
     &   call getmodel(phipar)

c
c     Get analysis voltage(s). 
c      
      if ((cfg(10).ne.'M').and.(cfg(10).ne.'C')) 
     &call getvolt(cfg(5),e0,voltages)

c     Same system but new voltages(s).
      
      if ((cfg(10).eq.'B').or.(cfg(10).eq.'E')) then
        if (cfg(5).eq.'N') call newvolt(nel,nels,symb,e0,mode)
        
c       Check Eo vs. Ec for each element, get new line, if necessary. 
        
        call newline(nels,symb,e0,ec,line)
      
      endif

c     Get the elements, lines, standards for a specimen analysis.      
      if (cfg(10).eq.'N') 
     &   call elements(nel,nels,symb,zx,ax,ec,e0,line,stds0,stds1,stds2,
     &  mode,standrd,nlayer,voltages)

c     Calculate the standard primary and x-ray secondary intensities. 
      call standard(nels,symb,ec,e0,line,fconts,fchars,
     &stds0,stds1,stds2,prims,phipar,toa,macchang,cfg(8))
      
c     Get Mass Abs. Coefficients for the specimen configuration. 
      call abscoeff(nel,nels,mac,symb,zx,ax,line,macchang,mode)
      
c     Get characteristic x-ray fluorescence constants.
      call charfdta(nels,zx,line,ax,ec,symb,e0,
     &exciter,fmac,phipar,'D',1)
      
c     Get layer densities. 
      if ((mode.eq.'F').and.(cfg(10).eq.'N')) call layrdens(rho,nlayer)
      
c     Is any layer fixed in composition and thickness?
100   call fixlayer(mode,cfg,nel,nels,delta,symb,c1,layrchar,ifix,rho)

c     Check if any element is analyzed by stoichiometry
c     if so, enter valences, but only for a new specimen configuration. 
      if ((cfg(1).eq.'N').and.(cfg(10).eq.'N')) 
     &call stoich0(ifix,nel,nels,st,symb,val)

c     Get kratios, or if calculating k-ratios for a given specimen,  
c     get layer thicknesses and compositions.
      call getkrat(mode,nel,nels,ifix,st,symb,layrchar,c1,kr,kf,
     &prims,fchars,fconts)

c     Check for analysis of the same element present in more than one layer.
      if (mode.eq.'F')
     &call mulayelm(symb,cfg(7),mle,nels,nel,nelt,mlen,ik)
c
      if ((cfg(3).eq.'K').or.(mode.eq.'B')) iter=-1

c     Calculate starting film thickness for iteration procedure.      
      if ((cfg(3).eq.'C').and.(mode.eq.'F'))
     &call strtthck(toa,ifix,nel,st,val,c1,c3,ax,e0,nels,delta,kr,mac)

c     Is an element analyzed by difference (bulk mode)? 
      if (mode.eq.'B') call differen(c1,symb,nels)

c     Normalize concentrations based on stoichiometries, if applicable. 
      if ((cfg(3).eq.'C').or.(mode.eq.'F'))
     &  call stoich1(ifix,nel,st,val,c1,c3,ax)

c     Initialize some more variables.
      call init5(mode,c1,c2,c3,c4,sc) 

c     Re-calculate initial thicknesses based on stoichiometries, if applicable. 
       if (mode.eq.'F') then
         do 183 kk=1,6
           if (st(kk).ne.0) delta(kk)=delta(kk)/(1.-c1(st(kk)))
183      continue
       endif

c        to observe compositions and film thicknesses at each loop of
c        the iteration procedure, compile program 'film' with /dlines switch
c        call dlines(iter,delta,cfg(3),mode,nels,nlayer,c3)

      ic=0

199   iter=iter+1
      notcnvrg=.false.
c
c Begin iteration procedure.
c
1991  do 300 ij=1,nels
        if (mode.eq.'F') i=nelt(ij)
        if (mode.eq.'B') i=ij
        
c
c Switch '!' indicates element composition has been fixed, calculate        
c a k-ratio for this element only at end of iteration procedure.
c        
       if (.not.solution) then
          if (symb(i)(6:6).eq.'!') goto 300
        else
          if (symb(i)(6:6).ne.'!') goto 300
        endif
c
c       Calculate continuum fluorescence intensities, if requested.
c
        if (cfg(8).eq.'Y') 
     &    call contflr(ij,nel,nels,zx,ec(ij),line,ax,e0(ij),c1,
     &    fcont(ij),phipar,toa,mac,delta,mode)
        
c        
c       Primary intensity constants.
c
        lea=nedge(line(i))
        fc1=effyld(zx(i),lea,e0(i),'electrons')
        fc2=trnsprob(zx(i),line(i))
        fc3=1./ax(i)
        fc4=znl(lea)
        fc5=qe0(ec(i),e0(i),symb(i)(3:5),zx(i),phipar)
        const=fc1*fc2*fc3*fc4*fc5
c        
c
c
        layer=layr(nel,i)
        chi(layer)=0.
c        
c Weighted mass absorption coefficients for all layers.
c
        do 200 j=1,nels
          layer2=layr(nel,j)
          if (layer.eq.layer2) chi(layer)=chi(layer)+c1(j)*mac(i,j)
200     continue
          
c          
c  Calculate phi(rz) function and integrate.
c
        if (phipar.eq.'E') then
          call pap(delta,i,nel,nels,zx,ax,c1,e0(i),ec(i),line(i),toa,
     &    mac,mode,za,a1,a2,b1,rc,rm,rx,symb(i),'I',z)
        else
          if (layer.ge.2) call chiov(mac,i,nel,layer,c1,chiovl)
          call phirzeq(nel,nels,i,delta,alpha,beta,gamma,phi0
     &    ,zx,ax,c1,e0(i),ec(i),phipar,line(i),.false.,z)
          if (mode.eq.'F') icase=layer
          if (mode.eq.'B') icase=8
          call integral(delta,icase,alpha,beta
     &      ,gamma,phi0,chi(layer),chiovl,za(1),toa)
        endif
c         
        prim(i)=const*c1(i)*za(1)
c
c       Correct for any characteristic x-ray fluorescence
c        
        call charflr(i,nel,nels,mode,zx,ax,symb,
     &  e0(i),c1,delta,fchar(i),phipar,toa,mac,exciter,fmac)
          
c
c       The k-ratio. 
c        
        krx(i)=(prim(i)+fchar(i)+fcont(i))/
     &           (prims(i,1)+fchars(i,1)+fconts(i,1))


c        
c Calculate thicknesses and concentrations for the next iteration. 
c        
        if (mode.eq.'F') then
          if (ifix(layer)) goto 300
c          
c         New concentrations.
c
          summle=0.
          do 244 kk=2,7
            summle=summle+krx(mle(kk))
244       continue
          if (i.eq.mle(1)) then
            c3(i)=(kr(i)-summle)/krx(i)*c1(i)
          else
            c3(i)=kr(i)/krx(i)*c1(i)
          endif

          c4(i)=c3(i)

c         Re-normalize compositions if stoichiometric analysis. 
          if (i.eq.st(layer)) call stoich1(ifix,nel,st,val,c4,c3,ax)

          sc(layer)=sc(layer)-c2(i)+c3(i)

c
c         New thicknesses. 
c
          do 250 j=1,6
            if (((layer.eq.j).and.(layr(nel,i+1).gt.j)).or.
     &          ((layer.eq.j).and.(layr(nel,i+2).gt.j).and.
     &          (i.eq.st(layer)).and.(cfg(7).eq.'Y')))
     &        delta(j)=sc(j)*delta(j)
250       continue

        else if (mode.eq.'B') then
          if (cfg(3).eq.'K') goto 300
          if (st(1).ne.i) then
            if (symb(i)(7:7).eq.'D') goto 298
            alp=(1.-krx(i))/krx(i)*c1(i)/(1.-c1(i))
            c3(i)=alp*kr(i)/(1.+kr(i)*(alp-1.))
          endif
298       c4(i)=c3(i)
          if (st(1).eq.i) call stoich1(ifix,nel,st,val,c4,c3,ax)
        endif

c       Test if the current approximation is same as last (within .0001)
c       if it is not, then set flag "notcnvrg" to goto next iteration.

        if (abs(c2(i)-c3(i)).gt..0001) notcnvrg=.true.
        if ((mode.eq.'F').and.(iter.lt.4)) notcnvrg=.true.
c
        if (iter.gt.21) notcnvrg=.false.
c
        c2(i)=c3(i)
300   continue
      if (cfg(3).eq.'C') then
        do 400 i=1,nels
          if (symb(i)(7:7).eq.'D') call differen(c3,symb,nels)
400     c1(i)=c3(i)
        
c       Re-normalize compositions if stoichiometric analysis. 
        call stoich1(ifix,nel,st,val,c1,c3,ax)

c       if (.not. solution)
c    &  call dlines(iter,delta,cfg(3),mode,nels,nlayer,c3)

        if (notcnvrg) goto 199
        if ((.not.solution).and.(mode.eq.'F')) then
          solution=.true.
          goto 1991
        endif
      endif

      print 967,7,7
967   format(2a1)
      if (cfg(3).eq.'K') iter=0

      summle=0.
      do 968 kk=2,7
        summle=summle+krx(mle(kk))
968   continue

      if (cfg(7).eq.'Y') kr(mle(1))=kr(mle(1))-summle
      do 970 i=1,nels
          cstd(i)=(prims(i,1)+fchars(i,1)+fconts(i,1))/
     &    (prims(i,2)+fchars(i,2)+fconts(i,2))
        if ((ifix(layr(nel,i))).or.(cfg(3).eq.'K').or.(i.eq.mle(1)))
     &     kf(i)=krx(i)*cstd(i)
970   continue

c      
c     Title for results.
c
      call titl(nel,nels,symb,e0,ifix,finame,phipar,iter,ianalys,
     &mode,standrd,stds0,cstd,val,mle,cfg)

c      
c     Print results.
c
      call printout(ifix,layrchar,nel,nels,c3,ax,prim,fcont,delta,
     &symb,rho,fchar,kf,kr,krx,cfg,mode,prims,fchars,fconts)

      if (ianalys.eq.1) print 972
972   format(' See file ''help4.dat'' for explanation of ''Fchar'' and '
     &'Fcont''.',/)

c      
c      Print primary and secondary x-ray intensities.
c
       if (cfg(9).eq.'Y')
     & call fluorprn(symb,nel,nels,stds0,standrd,prim,prims,
     &fcont,fconts,fchar,fchars,ianalys)

      ianalys=ianalys+1
c      
c     Get configuration of the next calculation.
c
      call nextcalc(cfg,mode)

      if ((cfg(1).eq.'Y').or.(cfg(2).eq.'Y')) goto 3
      if (cfg(4).eq.'Y') then
        if (cfg(10).eq.'N') then 
          goto 1
        else
          goto 2
        endif
      endif
      close (7,status='keep')
990   format (a1)
991   format (a3)
      end
