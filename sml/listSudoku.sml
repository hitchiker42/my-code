structure ListSudoku : SUDOKU = struct

type grid = int list

exception Parse
exception Unsolvable

fun parseString _ = raise Fail "not implemented"
fun get _ = raise Fail "not implemented"
fun set _ = raise Fail "not implemented"
fun unsolved _ = raise Fail "not implemented"
fun possibles _ = raise Fail "not implemented"
fun valid _ = raise Fail "not implemented"
fun sudoku _ = raise Fail "not implemented"

fun parseFile file =
    let open TextIO
        val s = openIn file
    in
      parseString (input s) before closeIn s
    end

local
    open TextIO
    fun ts 0 = "."
      | ts x = Int.toString x
    fun pr [] _ = ()
      | pr (x::l) n = (print (ts x);
                       if n mod 9 = 0 then print "\n" else print " ";
                       pr l (n+1))
in
fun print s = pr s 1
end

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
