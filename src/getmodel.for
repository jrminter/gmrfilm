c
c     Lists options for phi(rz) model and returns choice.
c     5/93 r.a. waldo
c
      subroutine getmodel(phipar)
      character*1 phipar,model
      print 100
100   format('+Choices of phi(rz) models are :',/)
110   print 120
120   format ('     Bastin''s    Scanning   (1986)     (b) ',/,
     &'        "        Scanning  (1990)      (c) ',/,
     &' Pouchou, Pichoir (PAP) Scanning (1990 (e)',/,
     &' or  Packwood''s  MAS        (1986)     (p)  ',/,
     &'                            Choose (default=e) : ')
      read 990,model
      if (model.eq.' ') model='e'
      if ((model.ne.'b').and.(model.ne.'B').and.(model.ne.'c').and.
     &    (model.ne.'C').and.(model.ne.'e').and.(model.ne.'E').and.
     &    (model.ne.'p').and.(model.ne.'P')) goto 110
      if (model.eq.'b') model='B'
      if (model.eq.'c') model='C'
      if (model.eq.'e') model='E'
      if (model.eq.'p') model='P'
      phipar=model
990   format(a1)      
      return
      end
