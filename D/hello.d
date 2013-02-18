import std.stdio;
int main(string[] args){
    string str="Hello,World!";
	writeln(str);
	return(0);
}
/*    int len=13;
    asm{
        mov EAX,4;
        mov EBX,1;
        mov ECX,str;
        mov EDX,len;
        int 80h;
        mov EAX,1;
        mov EBX,0;
        int 80h;
    };
    return(0);
}*/
