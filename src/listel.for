c------------------------------LISTEL---------------------------------
c
c     With 'titl' creates a title to be used in data output based on the
c     alphanumeric representations (atomic symbols) of all the
c     analyzed elements.
c     1/90 r.a. waldo
c
      subroutine listel(title,label,ll,lu,symb)
      character*76,title
      character*12 label
      character*7 symb(15)
      title(1:12)=label(1:12)
      ix=13
      do 10 i=ll,lu
        if (i .eq. ll) then
          title(ix:ix+5)=symb(i)(1:2)//';'//symb(i)(3:5)
          ix=ix+6
        else
          title(ix:ix+7)=', '//symb(i)(1:2)//';'//symb(i)(3:5)
          ix=ix+8
        endif
10    continue
      title(ix:76)=' '
      return
      end
