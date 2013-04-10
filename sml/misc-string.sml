structure MiscString = struct
val _ = use "misc-list.sml"
open MiscList
fun is_ws chr = ord chr = 0x20 || 0x09<=(ord chr) 
                                               && (ord chr)<=0x0D 
fun strip str =let
    val chars = explode str
    val f = (not o is_ws)
in List.filter f chars end
fun print_list (l:string list)=let
    val strings=ref l
in while not ((!strings)=[]) do
         print (concat ((des_car(strings))::"\n"::[])) end
fun $ (a, f) = f a
fun id x = x
structure Printf =
   struct
      fun fprintf out =
         Fold.fold ((out, id), fn (_, f) => f (fn p => p ()) ignore)

      val printf = fn z => fprintf TextIO.stdOut z

      fun one ((out, f), make) =
         (out, fn r =>
          f (fn p =>
             make (fn s =>
                   r (fn () => (p (); TextIO.output (out, s))))))

      val ` =
         fn z => Fold.step1 (fn (s, x) => one (x, fn f => f s)) z

      fun spec to = Fold.step0 (fn x => one (x, fn f => f o to))

      val B = fn z => spec Bool.toString z
      val I = fn z => spec Int.toString z
      val R = fn z => spec Real.toString z
   end
end
