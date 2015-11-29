#!/usr/bin/python3.2
from gi.repository import Gtk
from sys import argv
def Run(window):
    if window=='My':
        window=MyWindow
    elif window=='Grid':
        window=GridWindow
    window.connect("delete-event",Gtk.main_quit)
    window.show_all()
    Gtk.main()

def transpose(x):
    y=x
    for i in list(range(0,len(x))):
        for j in list(range(0,len(x[i]))):
            y[i][j]=x[j][i]
    return(y)

def Hello_World():
    class MyWindow(Gtk.Window):

        def __init__(self):
            Gtk.Window.__init__(self,title="Hello World")
            self.button=Gtk.Button(label="I'm A Button")
            self.button.connect("clicked",self.on_button_clicked)
            self.add(self.button)
        def on_button_clicked(self,widget):
            print("子に痴話　世界")


class MyWindow(Gtk.Window):

    def __init__(self):
        Gtk.Window.__init__(self,title="Hello World")
        self.box=Gtk.Box(spacing=6)
        self.add(self.box)
        self.button1=Gtk.Button(label="I'm A Button")
        self.button1.connect("clicked",self.on_button1_clicked)
        self.box.pack_start(self.button1,True,True,0)
        self.button2=Gtk.Button(label="Me Too")
        self.button2.connect("clicked",self.on_button2_clicked)
        self.box.pack_start(self.button2,True,True,0)
    def on_button1_clicked(self,widget):
        print("子に痴話　世界")
    def on_button2_clicked(self,widget):
        print("佐用野良")

class Board():
        board_init=[[' 0 ',' 0 ',' 0 '],[' 0 ',' 0 ',' 0 '],[' 0 ',' 0 ',' 0 ']]
        board=board_init
        board_inv_up=transpose
        board_inv=board_inv_up(board)
        turn=0
        def win(board):
            board_inv=Board.board_inv_up(board)
            for i in list(range(0,3)):
                if board[i][0]==board[i][1]==board[i][2]==' X ' or board_inv[i][0]==board_inv[i][1]==board_inv[i][2]==' X ':
                    Board.board=Board.board_init
                    return("X WINS")
                elif board[i][0]==board[i][1]==board[i][2]==' O ' or board_inv[i][0]==board_inv[i][1]==board_inv[i][2]==' O ':
                    Board.board=Board.board_init
                    return("O WINS")
            return(None)
        def print_board(board=Board.board):
            m=-1
            for i in board:
                print()
                m=m+1
                for j in board[m]:
                    print(j,end='')
            print()
        def mark(x,y):
            Board.print_board()
            if Board.win(Board.board)!=None:
                print(Board.win(Board.board))
                Board.board=Board.board_init
                Board.board_inv=Board.board_inv_up(Board.board)
                return
            elif Board.board[x][y]!=' 0 ':
                full=True
                for i in list(range(0,len(Board.board))):
                    for j in Board.board[i]:
                        if j==' 0 ':
                            full=False
                            break
                if full==True:
                    print("Game Tied Starting New Game")
                    Board.board=Board.board_init
                    Board.board_inv=Board.board_inv_up(Board.board)
                    return
                print("Illegal Move Pick Another Square")
                Board.turn=Board.turn+1
                return
            elif Board.turn%2==1:
                Board.board[x][y]=' X '
            else:
                Board.board[x][y]=' O '
            return

class GridWindow(Gtk.Window):
    def __init__(self):
            Gtk.Window.__init__(self,title="Tic Tac Toe")
            Board.turn=0
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
         Board.mark(0,0)
         Board.turn=Board.turn+1
    def ibutton2(self,widget):
         Board.mark(0,1)
         Board.turn=Board.turn+1
    def ibutton3(self,widget):
         Board.mark(0,2)
         Board.turn=Board.turn+1
    def ibutton4(self,widget):
         Board.mark(1,0)
         Board.turn=Board.turn+1
    def ibutton5(self,widget):
         Board.mark(1,1)
         Board.turn=Board.turn+1
    def ibutton6(self,widget):
         Board.mark(1,2)
         Board.turn=Board.turn+1
    def ibutton7(self,widget):
         Board.mark(2,0)
         Board.turn=Board.turn+1
    def ibutton8(self,widget):
         i8=Board.mark(2,1)
         Board.turn=Board.turn+1
    def ibutton9(self,widget):
         Board.mark(2,2)
         Board.turn=Board.turn+1


#lambda board:[[board[0][0],board[0][1],board[0][2]],[board[1][0],board[1][1],board[1][2]],[board[2][0],board[2][1],board[2][2]]]
if __name__=='__main__':
    Run(argv[1])
