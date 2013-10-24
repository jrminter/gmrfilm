c-----------------------------BASTBETA-------------------------------
c
c      this program finds the proper beta which fits the
c      conditions required for bastin's nbs workshop (1988) phi(rz) model
c      program completed 10/88 r.a. waldo

c   NOTE:see Scanning, vol. 12, 1990, p.225 for this model.

c   Note: bastbeta.tmp : see line change below; 5-21-90 r.a.w.
        subroutine bastbeta(a,beta,g,p,f,flag)
        logical flag
        flag=.false.
        x=(g-2.*a*f/sqrt(3.1415926))/(g-p)
        if ((x.gt.1.).or.(x.le.0.)) then
c   Note: bastbeta.ftn (Bastin's formulation) had the following two lines
c g=(g+p)/2.
c p=g
c could this change be more physically realistic
          g=p
          a=(g+p)/sqrt(3.1415926)/4./f
          x=0.5
          flag=.true.
        endif
        call rb2a(x,y1)
        beta=y1*(2*a)
1       beta1=beta
        y2=arfc(y1)/x*y1
        beta=y2*(2*a)
        if (abs(beta1-beta).lt.1.) return
        y1=y2
        goto 1
        end
