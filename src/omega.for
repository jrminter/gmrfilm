c-----------------------------OMEGA-----------------------------------
c Fluorescence Yields, from Krause, J. Phys. Chem. Ref. Data
c                           Vol. 8, No. 2, 1979, p. 307.
c
c 3/91 r.a. waldo
c
      function omega(z,nedge)
      real l1omega(92),l2omega(92),l3omega(92)
      data l1omega/11*0.0,.000029,.000026,.00003,.000039,.000074,
     & .00012,.00018,.00024,.00031,.00039,.00047,.00058,.00071,
     & .00084,.001,.0012,.0014,.0016,.0018,.0021,.0024,.0028,.0032,
     & .0036,.0041,.0046,.0051,.0059,.0068,.0094,.01,.011,.012,.013,
     & .014,.016,.018,.02,.037,.039,.041,.044,.046,.049,.052,.055,
     & .058,.061,.064,.066,.071,.075,.079,.083,.089,.094,.1,.106,
     & .112,.12,.128,.137,.147,.144,.130,.120,.114,3*.107,.112,.117,
     & .122,.128,.134,.139,.146,.153,.161,.162,.176/
      data l2omega/11*0.0,.0012,.00075,.00037,.00031,.00026,.00024,
     & .00022,.00027,.00033,.00084,.0015,.0026,.0037,.005,.0063,.0077,
     & .0086,.010,.011,.012,.013,.014,.016,.018,.020,.022,.024,.026,
     & .028,.031,.034,.037,.040,.043,.047,.051,.056,.061,.065,.069,.074,
     & .079,.083,.090,.096,.103,.11,.117,.124,.132,.140,.149,.158,
     & .167,.178,.189,.2,.211,.222,.234,.246,.258,.27,.283,.295,.308,
     & .321,.334,.347,.36,.373,.387,.401,.415,.429,.443,.456,.468,
     & .479,.472,.467/
      data l3omega/11*0.0,.0012,.00075,.00038,.00031,.00026,.00024,
     & .00022,.00027,.00033,.00084,.0015,.0026,.0037,.005,.0063,.0077,
     & .0093,.011,.012,.013,.015,.016,.018,.020,.022,.024,.026,.028,
     & .031,.034,.037,.040,.043,.046,.049,.052,.056,.06,.064,.069,.074,
     & .079,.085,.091,.097,.104,.111,.118,.125,.132,.139,.147,.155,.164,
     & .174,.182,.192,.201,.21,.22,.231,.243,.255,.268,.281,.294,.306,
     & .32,.333,.347,.36,.373,.386,.399,.411,.424,.437,.45,.463,.476,
     & .489/
       goto (1,2,3,4,4,5,5,6,6,7,5,5,5),nedge
1      d=(.015+.0327*z-.64e-6*z**3)**4
       omega=d/(1.+d)
       return
2      omega=l1omega(int(z))
       return
3      omega=l2omega(int(z))
       return
4      omega=l3omega(int(z))
       return
5      omega=0.
       return
6      omega=.68e-9*(z-13)**4
       return
7      omega=1.e-9*(z-13)**4
       return
       end
