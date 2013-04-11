(*
signature SUDOKU = sig
eqtype grid
exception Parse
exception Unsolvable
val parseString: string -> grid
val parseFile: string -> grid
val get: grid * int -> int
val set: grid * int * int -> grid (* may modify the grid, or not *)
val unsolved: grid -> int option
val possibles: grid * int -> int list
val valid: grid -> bool
val print: grid -> unit
val sudoku: grid -> grid
end*)

structure ListSudoku (*: SUDOKU*) = struct
(*grid is an array of int,int list pairs where the int is 0 for unsolved cells
 *and 1-9 for solved cells(where the value is the value of the solved cell)
 *the int list is [] for solved cells and an array of possible values for
 *an unsolved cell,as an example take the row 1.3.5.62.8
 *were there no other constraints this would be represented by
 *[(1,[]),(0,[4,7,9]),(3,[]),(0,[4,7,9]),...
 *(5,[]),(6,[]),(2,[]),(0,[4.7.9]),(8,[])]*)
type grid = (int*int list) array

exception Parse
exception Unsolvable
val _ = use "load.sml"
val Get = Array.sub
val Set = Array.update
val seq9 = MiscList.seq 1 9
val slice = ArraySlice.slice
val check = fn i=> if i<0 || i>80 then raise Subscript else ()
fun init () = Array.array(81,(0,[]))
fun get (arr,i) = (check i;fst(Get (arr,i)))
fun set (arr,i,x) = (check i;Set(arr,i,(x,[]));arr)
(*ignore all whitespace,(run strip on given string)
run explode on string, get char array, if array not len 81 raise exception
for index i if 0x31 <= chr <= 0x39 then set(grid,i,(chr,[]))
else set(grid,i,(0,[1,2,3,4,5,6,7,8,9]))*)
fun parseString str = let
    val chars = MiscString.strip str
    val chrs = Array.fromList chars
    val g = init ()
    fun set i = let val c=ord (Get (chrs,i)) in
        if 0x31<=c && c<=0x39 then Set (g,i,((c-0x30),[]))
                else Set (g,i,(0,seq9)) end
    fun loop i = if i=0 then () else (set (i-1);loop (i-1))
in if Array.length chrs != 81 then raise Parse else (loop 81;g) end
(* if for some element i in grid #1(i) = 0 then SOME(i)
   else NONE*)
fun unsolved board = let
    fun for ~1 arr = NONE
    | for i arr = if (fst(Get(arr,i)) = 0) then SOME(i) else for (i-1) arr
in for 80 board end
fun possibles (arr,i) =
    (check i;if fst((Get (arr,i)))>0 then fst((Get (arr,i)))::[]
                                 else snd((Get (arr,i))))
fun parseFile file =
    let open TextIO
        val s = openIn file
    in
      parseString (input s) before closeIn s
    end
(*fun row i a = sl_to_list (slice(a,(i-9),Some(i)))*)
(*return row indices, rows go from 0-8*)
fun row i = MiscList.seq (i*9) (((i+1)*9)-1)
(*return incices in column j, collumns are numbered 0-8*)
fun column j  = let
    val inc = fn n => n+9
    val test = fn n => n>=81
    fun loop n f g x = if f n then rev x
                         else loop (g n) f g (n::x)
in loop j test inc [] end
(*return the indices of a block, blocks are numbered:
|1|2|3|
|4|5|6|
|7|8|9|*)
fun block k  = let
    val j = (if (k % 3) = 0 then 0 else if (k % 3) = 1 then 3
            else 6)
    val i = (if j = 0 then k else if j = 3 then k-1 else k-2)
(*I'm too lazy to think of a function to do this*)
in (9*(i)+j)::(9*(i)+j+1)::(9*(i)+j+2)::(9*(i+1)+j)::(9*(i+1)+j+1)::(9*(i+1)+j+2)::(9*(i+2)+j)::(9*(i+2)+j+1)::(9*(i+2)+j+2)::[] end

(*run function f over all rows collums and blocks in the board
 *for this to be of any worth function f must opperate by side effect only*)
fun board_loop b f = let
    fun iter 0 0 = []
      | iter 0 j = iter 8 (j-1)
      | iter i j = case j of
                       1 => (f b (block i);iter (i-1) j)
                         | 2 => (f b (column i);iter (i-1) j)
                             | 3 => (f b (row i);iter (i-1) j)
in iter 8 3 end
(*test if board solved correctly*)
exception Unsolved
fun check b = let
    fun chk b rcb = let
        fun complete [] l =
            if (MiscList.quickSort (op <) l) = (MiscList.seq 1 9)
                        then () else raise Unsolved
      | complete (x::xs) l = complete xs ((fst(Get(b,x)))::l)
    in complete rcb [] end
in board_loop b chk end
fun singletons a [] = ()
  | singletons a (l::ls) = (if len (snd(Get(a,l))) = 1
                            then Set(a,l,(hd(snd(Get(a,l))),[]))
                            else ();singletons a ls)
(*set any value in row/column/block represented by l that is 'forced'
 *to be a certain value. where forced is defined as a space needing to
 *be a specific number to fufill the 1-9 rule despite having multiple possible
 *values remaining, ie. if space 5 is the only space in row 1 with an 8 it
 *its list of possibles it is forced to be 8, regardless of weather or not it
 *has other posable values*)
fun forced a l = let
    val collect = Array.array (9,[])
    (*want to do, for i in snd(l) do for j in i
        do set collect[i]=fst(l)::collect[i]
      for i j in enum collect do if len j = 1 set fst a[hd j] to i
          and set snd a[hd j] to [] and update board*)
(*here i is a list of indices in a*)
    val q = ref 0
    fun fori [] = ()
      | fori (x::xs) = let
          val ls = snd(Get(a,x))(*array of possibles in a[x]*)
          fun forj [] = ()
            | forj (y::ys) =
              (Set (collect,(!q),(y::(Get(collect,(!q)))));forj ys)
      in (forj ls;q+=1;fori xs) end
    fun forc ~1 = ()
      | forc i = let
          val c = Get (collect,i)
      in (if len c = 1 then Set (a,(hd c),(i,[])) else ();forc (i-1))end
in (fori l;forc 8) end
fun getRCB i = let
    val triple = Array.array (3,9)
    val rows = Array.fromList (List.map row (MiscList.seq 0 8))
    val cols = Array.fromList (List.map column (MiscList.seq 0 8))
    val blks = Array.fromList (List.map block (MiscList.seq 0 8))
    fun getsec x j ~1 = raise Subscript
    | getsec x j i = if MiscList.contains (Get (x,i)) j then i
                     else getsec x j (i-1)
in [(getsec rows i 8),(getsec cols i 8),(getsec blks i 8)] end
(*update possible values for all undecided cells in the same
 *row,collumn and block as i*)
fun update a i = let
    val x = fst(Get(a,i))
    val RCB = ref (getRCB x)
    val row = row (MiscList.popl RCB)
    val col = column (MiscList.popl RCB)
    val blk = block (MiscList.popl RCB)
    val check = fn k => k = x
    val update = fn n=>Set (a,n,(fst(Get (a,n)),
                                  List.filter check (snd(Get(a,n)))))
    fun for j (l::ls) = if j = ~1 then () else (update l;for (j-1) ls)
in (for 8 row;for 8 col;for 8 blk) end
fun main board =
    (*board-old=copy board*)
    (*for 0-8 in rows,columns,blocks do singletons*)
    (*if board!=board-old goto start*)
    (*for 0-8 in rows,columns,blocks do forced*)
    (*then guess and check*)
while (unsolved board != NONE) do
      let val old = MiscList.clone board
          val _ = board_loop board singletons
      in if board = old then (board_loop board forced;
                              if board = old then
                              (*guess*)() else ())
         else () end
(*true iff grid filled and consistant*)
fun valid board = (check board;true)
                  handle Unsolved => false
(*Solves given grid*)
fun sudoku board = main board
local
    open TextIO
    fun ts 0 = "."
      | ts x = Int.toString x
    fun pr arr 81 = ()
      | pr arr n = (print (ts (fst(Get(arr,n))));
                       if n mod 9 = 0 then print "\n" else print " ";
                       pr arr (n+1))
in fun print s = pr s 1 end
end
(*Sudoku Solver in python*)

(*import sys, math, time

def display(board):
    for row in board:
        print " ".join(row)
    
def in_column(num, cix, board):
    for rix in range(0, size):
        if num == board[rix][cix]:
            return 1

def in_square_regular(num, rix, cix, board, rwidth, cwidth):
    roff = rix % rwidth
    rlow, rhigh = rix - roff, rix + (rwidth - roff)
        
    coff = cix % cwidth
    clow, chigh = cix - coff, cix + (cwidth - coff)

    for rix in range(rlow, rhigh):
        for cix in range(clow, chigh):
            if board[rix][cix] == num:
                return 1
            
def callnext(rix, cix, board):
    if cix + 1 != size:
        search(rix, cix + 1, board)
    elif rix + 1 != size:
        search(rix + 1, 0, board)
    else:
        print "===== A SOLUTION ====="
        display(board)
    
def search(rix, cix, board):
    if board[rix][cix] == ".":
        for num in possibles[rix][cix]: #candidates
            if not num in board[rix] and \
               not in_column(num, cix, board) and \
               not in_square(num, rix, cix, board):
                board[rix][cix] = num
                callnext(rix, cix, board)
        board[rix][cix] = "."
    else:
        callnext(rix, cix, board)

def prepare(board):
    found = 1
    total = 0
    while found:
        find_possibles(board)
        found = find_singletons(board)
        print "SINGLETONS:", found
        find_possibles(board)
        found2 = cross_check(board)        
        print "CROSS CHECKED:", found2
        found = found + found2
        same_in_range(possibles)
        restricted_to_rorc(possibles)
        total += found
    print "TOTAL:", total

def find_possibles(board):
    for rix in range(size):
        for cix in range(size):
            if board[rix][cix] == ".":
                possible = []
                for x in possibles[rix][cix]:
                    if not x in board[rix] and \
                       not in_column(x, cix, board) and \
                       not in_square(x, rix, cix, board):
                        possible.append(x)

                possibles[rix][cix] = possible
            else:
                possibles[rix][cix] = []

def find_singletons(board):
    found = 0
    for rix in range(size):
        for cix in range(size):
            if board[rix][cix] == "." and len(possibles[rix][cix]) == 1:
                board[rix][cix] = possibles[rix][cix][0]
                found += 1
                print "Singleton at %s, %s: %s" % (rix, cix, board[rix][cix])
                possibles[rix][cix] = []
    return found

def add(map, pos, candidate):
    list = map.get(candidate, [])
    if not list:
        map[candidate] = list
    list.append(pos)

def add_nodup(map, pos, candidate):
    list = map.get(candidate, [])
    if not list:
        map[candidate] = list
    if not pos in list:
        list.append(pos)
    
def cross_check(board):
    found = 0

    # check rows
    for rix in range(size):
        pos = {}
        for cix in range(size):
            for c in possibles[rix][cix]:
                add(pos, cix, c)
        for c in pos.keys():
            if len(pos[c]) == 1:
                cix = pos[c][0]
                print "ROW FOUND: (%s, %s) -> %s" % (rix, cix, c)
                board[rix][cix] = c
                possibles[rix][cix] = []
                found += 1

    find_possibles(board)
                
    # check columns
    for cix in range(size):
        pos = {}
        for rix in range(size):
            for c in possibles[rix][cix]:
                add(pos, rix, c)
        for c in pos.keys():
            if len(pos[c]) == 1:
                rix = pos[c][0]
                print "COL FOUND: (%s, %s) -> %s" % (rix, cix, c)
                board[rix][cix] = c
                possibles[rix][cix] = []
                found += 1

    # we don't implement squares on 6es correctly, so skip it
    if size == 6:
        return found
                
    find_possibles(board)

    # check squares
    width = int(math.sqrt(size))
    for rs in range(width):
        for cs in range(width):
            pos = {}
            for rix in range(rs * width, (rs+1) * width):
                for cix in range(cs * width, (cs+1) * width):
                    for c in possibles[rix][cix]:
                        add(pos, (rix, cix), c)
                        
            for c in pos.keys():
                if len(pos[c]) == 1:
                    (rix, cix) = pos[c][0]
                    print "SQUARE FOUND: (%s, %s) -> %s" % (rix, cix, c)
                    board[rix][cix] = c
                    possibles[rix][cix] = []
                    found += 1
    
    return found

def build_ranges_list(rows = 1, columns = 1, squares = 1):
    ranges = []
    # add rows
    if rows:
        for rix in range(size):
            row = []
            for cix in range(size):
                row.append((rix, cix))
            ranges.append(row)

    # add columns
    if columns:
        for cix in range(size):
            column = []
            for rix in range(size):
                column.append((rix, cix))
            ranges.append(column)

    # add squares
    if size != 6 and squares:
        width = int(math.sqrt(size))
        for sr in range(width):
            for sc in range(width):
                square = []
                for srix in range(width):
                    for scix in range(width):
                        rix = sr * width + srix
                        cix = sc * width + scix
                        square.append((rix, cix))
                ranges.append(square)

    return ranges

def same_in_range(possibles):
    """if two squares within a range contain the same *two* possibilities,
    those two are not possible elsewhere in that range. similarly,
    three and three, 4&4, ... """
    for range in ranges:
        alts = {}
        for (rix, cix) in range:
            if possibles[rix][cix]:
                key = "".join(possibles[rix][cix])
                add(alts, (rix, cix), key)

        if len(alts) == 1:
            continue
        
        for key in alts.keys():
            if len(key) == len(alts[key]):
                alternatives = alts[key]
                for cell in range:
                    if cell not in alternatives:
                        (rix, cix) = cell
                        for choice in key:
                            if choice in possibles[rix][cix]:
                                print "%s not possible at (%s, %s) (SAME)" % \
                                    (choice, rix, cix)
                                possibles[rix][cix].remove(choice)

def restricted_to_rorc(possibles):
    """if, for a square, a number can only occur in a certain row or
    column, it cannot occur in that row/column in other squares"""
    for square in squares:
        rows = {}
        cols = {}
        for (rix, cix) in square:
            for choice in possibles[rix][cix]:
                add_nodup(rows, rix, choice)
                add_nodup(cols, cix, choice)

        for choice in rows.keys():
            if len(rows[choice]) == 1:
                rix = rows[choice][0]
                for cix in range(size):
                    if choice in possibles[rix][cix] and \
                        (rix, cix) not in square:
                        print "%s not possible at (%s, %s) (RESTRICT)" % \
                            (choice, rix, cix)
                        possibles[rix][cix].remove(choice)

        for choice in cols.keys():
            if len(cols[choice]) == 1:
                cix = cols[choice][0]
                for rix in range(size):
                    if choice in possibles[rix][cix] and \
                       (rix, cix) not in square:
                        print "%s not possible at (%s, %s) (RESTRICT)" % \
                            (choice, rix, cix)
                        possibles[rix][cix].remove(choice)

# --- Main
        
board = [line.strip() for line in open(sys.argv[1]).readlines()]
size = len(board)

if size == 6:
    in_square = lambda n, r, c, b: in_square_regular(n, r, c, b, 2, 3)
    candidates = "123456"
elif size == 9:
    in_square = lambda n, r, c, b: in_square_regular(n, r, c, b, 3, 3)
    candidates = "123456789"
elif size == 16:
    in_square = lambda n, r, c, b: in_square_regular(n, r, c, b, 4, 4)
    candidates = "0123456789ABCDEF"
else:
    assert 0

board = [list(row) for row in board if len(row) == size]
assert len(board) == size

display(board)
possibles = []
for rix in range(size):
    row = []
    for cix in range(size):
        row.append(candidates[:])
    possibles.append(row)
start = time.clock()
ranges = build_ranges_list()
squares = build_ranges_list(rows = 0, columns = 0)
find_possibles(board)
prepare(board)
display(board)
search(0, 0, board)
print "Time taken:", time.clock() - start*)
(* Ideas in py program, I think
-variable solved 9*9
-Build up list of all values in all open cells,
filled in cells get the empty list & for each filled in cell solved--
-Check through all cells, if cell has only one possible value fill in cell
set to empty list && solved -- && clear that value from all cells in the
same row collunm & square
-for row,collumn and square check if only one open cell has a needed value
(ie if we have 4 open cells in a row with possibilities (1,2,3),(1,3,4),(1,2,5,4)&(1,2) then cell 3 must be 5(cell 2 must then be 4, but we'll catch that later)
-if puzzle not solved by this point then do guessing w/backtracking
*)
