c--------------------------------CK--------------------------------c Coster-Kronig Coefficientsc From Krause, J. Phys. Chem. Ref. Datac              Vol. 8, No. 2, 1979, p. 307.c      function ck(j,z)      real ck12(92),ck23(92),ck13(92)      DATA ck12/11*.0,6*.32,7*.31,5*.30,2*.29,4*.28,     &   3*.27,2*.26,9*.10,2*.17,2*.18,18*.19,2*.18,.17,     &   2*.16,.15,2*.14,2*.13,.12,2*.11,3*.10,3*.09,2*.08/      DATA ck23/27*0.0,2*.028,.026,.032,.050,.063,.076,     &   .088,.1,.109,.117,.126,.132,.137,.141,.144,.148,.15,.151,     &   .153,.155,2*.157,.156,.155,3*.154,4*.153,.152,.151,.150,     &   .149,.147,.145,.143,.142,.14,.139,.138,.136,.135,.134,.133,     &   .13,.128,.126,.124,.122,.12,.118,.116,.113,2*.111,.11,.109,     &   3*.108,.139,.167/      DATA ck13/11*0.0,3*.64,.63,4*.62,.61,.6,.59,.58,.57,.58,     &    .57,.56,.55,2*.54,3*.53,7*.52,4*.61,2*.6,3*.59,.27,6*.28,     &    3*.29,9*.3,2*.29,4*.28,.33,.39,.45,.5,.53,.56,.57,3*.58,     &    .59,4*.58,.57,.58,.57/        nz=int(z)        if (j.eq.12) ck=ck12(nz)        if (j.eq.13) ck=ck13(nz)        if (j.eq.23) ck=ck23(nz)        end