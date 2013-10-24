c---------------------------------NEDGE----------------------------------
c  Number of edge according to numbering system.
c
c 3/91 r.a. waldo
      function nedge(l)
      goto (1,1,2,2,2,3,3,4,4,7,8,9)l
1     nedge=1
      return
2     nedge=2
      return
3     nedge=3
      return
4     nedge=4
      return
7     nedge=7
      return
8     nedge=8
      return
9     nedge=9
      return
      end
