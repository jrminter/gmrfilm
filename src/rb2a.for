c-----------------------------RB2A---------------------------------
c
c     along with bastbeta used to find the
c     proper beta in bastins's nbs workshop (1988) phi(rz) model
c     10/88, r.a. waldo
c     NOTE: see also Scanning, Vol.12, 1990, p225.
c
      subroutine rb2a(x,b)
      if(x.lt..03165) then 
        b=(1.-4.894396*x)/(1.341313*x)
      else if(x.lt..056) then
        b=(1.-2.749786*x)/(1.447465*x)
      else if(x.lt..102) then
        b=(1.-1.043744*x)/(1.604820*x)
      else if(x.lt..306) then
        b=(1.-.5379956*x)/(1.685638*x)
      else if(x.lt..57) then
        b=4.852357*exp(-3.680818*x)
      else if(x.lt..7) then
        b=5.909606*exp(-4.015891*x)
      else if(x.lt..8) then
        b=13.4381*exp(-5.180503*x)
      else if(x.lt..9) then
        b=1.122405-1.141942*x
      else
        b=.9628832-.9642440*x
      endif
      return
      end
