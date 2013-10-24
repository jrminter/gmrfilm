c--------------------------------ARFC----------------------------------
c     
c     this program calculates the 'arfc' complementary error function
c     for the value x using horner's approximation method
c     for more efficient calculations, as suggested by Packwood,
c     'arfc' is erfc without the exp(-x*x) factor
c     program completed 4/87 by richard a. waldo

      function arfc(x)
      p=.3275911
      t=1./(1.+p*x)
      a1=.254829592
      a2=-.284496736
      a3=1.421413741
      a4=-1.453152027
      a5=1.061405429
      arfc=(a1*t+a2*t*t+a3*t*t*t+a4*t*t*t*t+a5*t*t*t*t*t)
      return
      end
