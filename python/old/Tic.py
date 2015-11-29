#!/usr/bin/python3.2
#IT'S MOTHERFUCKING TIC-TAC-TOE BITCHES

from gi.repository import Gtk
def Run():
    window=GW()
    window.connect("delete-event",Gtk.main_quit)
    window.show_all()
    Gtk.main()

def transpose(x):
    y=x[:]
    for i in list(range(0,len(x))):
        for j in list(range(0,len(x[i]))):
            y[i][j]=x[j][i]
    return(y)


class Board():
    board_init=[['   ','   ','   '],['   ','   ','   '],['   ','   ','   ']]
    board=board_init[:][:]
    board_inv=transpose(board[:][:])
    def win(board):
        bl=board[:][:]
        bl_inv=transpose(bl)
        for i in list(range(0,3)):
            if bl[i][0]==bl[i][1]==bl[i][2]==' X ' or bl_inv[i][0]==bl_inv[i][1]==bl_inv[i][2]==' X ':
                Board.board=Board.board_init[:]
                return("X WINS")
            elif bl[0][0]==bl[1][1]==bl[2][2]==' X ' or bl_inv[0][2]==bl_inv[1][1]==bl_inv[2][0]==' X ':
                Board.board=Board.board_init[:]
                return("X WINS")
            elif bl[i][0]==bl[i][1]==bl[i][2]==' O ' or bl_inv[i][0]==bl_inv[i][1]==bl_inv[i][2]==' O ':
                Board.board=Board.board_init[:]
                return("O WINS")
            elif bl[0][0]==bl[1][1]==bl[2][2]==' O ' or bl_inv[0][2]==bl_inv[1][1]==bl_inv[2][0]==' O ':
                Board.board=Board.board_init[:]
                return("O WINS")
            return(None)
    def print_board(board):
        m=-1
        for i in board:
            print()
            m=m+1;n=-1
            if m>0:
                print('-----------')
            for j in board[m]:
                n=n+1
                print(Board.board[m][n],end='',sep='')
                if n<2:
                    print('|',end='',sep='')
        print()
    def mark(x,y,turn):
        m=-1
        bl=Board.board[:][:]
        if not bl[x][y]=='   ':
            full=True
            for i in bl:
                m=m+1
                n=-1
                for j in bl[m]:
                    n=n+1
                    if bl[m][n]=='   ':
                        full=False
                        break
            if full==True:
                print("Game Tied")
                exit()
            print("Illegal Move Pick Another Square")
            GW.turn=GW.ii(turn)
        else:
            Board.board[x][y]=turn
            Board.print_board(Board.board)
        if Board.win(Board.board)!=None:
            print(Board.win(Board.board))
            Board.board=Board.board_init[:][:]
            Board.board_inv=transpose(Board.board[:][:])
            exit()

class GW(Gtk.Window):
    turn=' X '
    ii=lambda turn:' O ' if turn==' X ' else ' X '
    def __init__(self):
            Gtk.Window.__init__(self,title="Tic Tac Toe")
            grid=Gtk.Grid()
            self.add(grid)
            box=Gtk.Button
            pos=Gtk.PositionType
            self.button1=box(label="NW")
            self.button1.connect("clicked",self.ibutton1)
            self.button2=box(label="N")
            self.button2.connect("clicked",self.ibutton2)
            self.button3=box(label="NE")
            self.button3.connect("clicked",self.ibutton3)
            self.button4=box(label="W")
            self.button4.connect("clicked",self.ibutton4)
            self.button5=box(label="C")
            self.button5.connect("clicked",self.ibutton5)
            self.button6=box(label="E")
            self.button6.connect("clicked",self.ibutton6)
            self.button7=box(label="SW")
            self.button7.connect("clicked",self.ibutton7)
            self.button8=box(label="S")
            self.button8.connect("clicked",self.ibutton8)
            self.button9=box(label="SE")
            self.button9.connect("clicked",self.ibutton9)
            grid.attach(self.button1,0,0,6,6)
            grid.attach_next_to(self.button2,self.button1,pos.RIGHT,6,6)
            grid.attach_next_to(self.button3,self.button2,pos.RIGHT,6,6)
            grid.attach_next_to(self.button4,self.button1,pos.BOTTOM,6,6)
            grid.attach_next_to(self.button5,self.button4,pos.RIGHT,6,6)
            grid.attach_next_to(self.button6,self.button5,pos.RIGHT,6,6)
            grid.attach_next_to(self.button7,self.button4,pos.BOTTOM,6,6)
            grid.attach_next_to(self.button8,self.button7,pos.RIGHT,6,6)
            grid.attach_next_to(self.button9,self.button8,pos.RIGHT,6,6)
    def ibutton1(self,widget):
        Board.mark(0,0,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton2(self,widget):
        Board.mark(0,1,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton3(self,widget):
        Board.mark(0,2,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton4(self,widget):
        Board.mark(1,0,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton5(self,widget):
        Board.mark(1,1,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton6(self,widget):
        Board.mark(1,2,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton7(self,widget):
        Board.mark(2,0,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton8(self,widget):
        i8=Board.mark(2,1,GW.turn)
        GW.turn=GW.ii(GW.turn)
    def ibutton9(self,widget):
        Board.mark(2,2,GW.turn)
        GW.turn=GW.ii(GW.turn)

if __name__=='__main__':
    Run()
    
