local
  open ArraySlice
  fun sl_swap i j k l= let
    val tempi = ArraySlice.sub(i,k)
    val tempj= ArraySlice.sub(j,l)
  in (ArraySlice.update(i,k,tempj);ArraySlice.update(j,l,tempi)) end
in
fun mergesort_complex (filt,a:'a array) 
    = let
        fun slice (sl,i,sz)=ArraySlice.subslice(sl,i,SOME(sz))
        val arr=ArraySlice.full(a)
        fun merge l r sl = let
            val i = ref ((if len l > len r then len r else len l)-1)
        in
            (while (!i)>=0 do
                         if filt(sub(l,(!i))sub(r,(!i)))
                         then (Misc.sl_swap l r (!i) (!i);i:=((!i)-1))
                         else i:=((!i)-1);slice(sl,#2(base l),op+(#2(base l),#2(base r))))
        end
        fun mergesort sl = if (len sl) = 1 then sl else if (len sl) = 0 then (return;raise List.Empty) else
                           let
                               val m = op div((len sl),2)
                               val n = (len sl)
                               val l = slice(sl,0,(m-1))
                               val _ = print (Int.toString n)
                               val _ = print "\n"
                               val r = ArraySlice.subslice(sl,m,(NONE:int option))
                               val l = mergesort l
                               val r = mergesort r
                               val last_l=get(l,(len l)-1)
                               val hd_r = get(r,0)
                           in if (filt(hd_r,last_l)) then sl
                                  else  merge l r sl end
in (mergesort arr;Misc.arr_to_list return)end
fun mergeSort filt lst = let
    val filt = filt
    val len = List.length
    val lst=lst
    fun merge (l,r) = let
        val res = ref []
        fun merge2 [] [] x = rev (!x)
          | merge2 l [] x = rev (l@(!x))
          | merge2 [] r x = rev (r@(!x))
          | merge2 (l::ls) (r::rs) x = if (filt (l,r)) then
                                           (x:=(l::(!x));merge2 ls (r::rs) x)
                                       else (x:=(r::(!x));merge2 (l::ls) rs x)
in merge2 l r res end
    fun mergesort l = if len l = 1 then l else
                      let val mid = len l div 2
                          val left = ref (List.take (l,mid))
                          val right = ref (List.drop (l,mid))
                      in (left:=mergesort (!left);
                          right:=mergesort (!right);
                          if filt(List.last (!left),hd (!right))
                          then (!left)@(!right) else
                          merge ((!left),(!right))) end
in mergesort lst end
