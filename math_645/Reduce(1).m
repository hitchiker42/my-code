function AEQ = lzreduce(A,r)             %last updatedby D. Hill 2/3/92
%REDUCE  Perform row reduction on matrix A by explicitly choosing
%        row operations to use. A row operation can be "undone", but
%        this feature can not be used in succession.
%
%  Use in the form ---> reduce(A,'r')  for a rational display format
%   or in the form ---> reduce(A)   for a decimal display format
%
%  By: David R. Hill, Mathematics Dept., Temple Univ.
%      Philadelphia, PA. 19122
%
%  Revised on March 14, 1994 by Lee L. Zia, Dept. of Mathematics, UNH
%  Durham, NH 03824. Revision casts the basic elementary row operation
%  as: ``subtract a multiple(l_ij) of row j from row i'' (see Strang).
%
%  Further revisions (in the text strings only) on Sept. 1, 1994
%  by Kevin M. Short, Dept. of Mathematics, UNH, Durham, NH 03824
%  The only changes were to make the text prompts more specific,
%  at the expense of adding several more messages.
%
%  Usage: --when prompted for first row: enter row number j
%         --when prompted for row that changes: enter row number i
%
if nargin<2,rsig='F';else,rsig='T';end
[m,n]=size(A);
oldA=A;
myeps=1e-14; %my tolerance for zero in rational display
%Set up strings to be used as messages.
s0='     ';
s1='Enter number of the row that you want to multiply by the MULTIPLIER.  ';
s2='Enter second row number.  ';
s3='Enter MULTIPLIER.  ';
s4='Enter nonzero MULTIPLIER.  ';
s5='Enter row number.  ';
s6='Enter number of row that changes.  ';
s7='Last row operation "undone". ';
s8='               ***** "REDUCE" a Matrix by Row Reduction *****';
s9='         The current matrix is:';
s10='     ENTER your choice ---> ';
s11='Press  ENTER  to continue';
s12='*****  -->  REDUCE is over. Your final matrix is:';
s13='** Improper row number! **';
s14='Interchange Complete: ';
s15='Row Multiplication Complete: ';
s16='Replacement by Linear Combination Complete: ';
s17='Enter the number of one of the rows that you want to swap. ';
s18='Enter the number of the other row that you want to swap. ';
s19='For this option you will be asked for a MULTIPLIER which will then ';
s20='be multiplied by every element in the row you specify next.  The ';
s21='multiple of the row will then be SUBTRACTED from the last row you ';
s22='enter (i.e., the row that changes).  The first row you entered ';
s23='remains unchanged. ';

arrow=[setstr(60) setstr(196) setstr(62)];
menu=...
['            OPTIONS                                                    ';
 ' <1>  Interchange two rows.                      <4>  Turn on rational ';
 ' <2>  Multiply a row by a nonzero scalar.             display.         ';
 ' <3>  Replace:  row i by (row i - mult*row j)                          ';
 '                                                 <5>  Turn off rational';
 '<-1>  "Undo" previous row operation.                  display.         ';
 ' <0>  Quit reduce!                                                     '];
mess=s8;
i=sqrt(-1); 
while 1
  %cla
  disp(mess)
  disp(s9)
  a=A; %to prevent case mismatch in multiplier choice
%  if rsig=='T',Amat=A;A=rat(A,'s'),A=Amat;else,A,end
  if rsig=='T',Amat=A;format rat,A=Amat,A;else,format long,A,end
  disp(menu)
  ch=input(s10);
    if ch==-1
    A=oldA;
    mess=s7;
  end
  if ch==0
    %cla
    disp(s12)
    disp(s0)
%    if rsig=='T',Amat=A;A=rat(A,'s'),A=Amat;else,A,end
    if rsig=='T',Amat=A;format rat,A=Amat,A;else,format long,A,end
    AEQ=A;
    break
  end
  if ch==4, rsig='T';end
  if ch==5, rsig='F';end
  if ch==1
    sig='Y';
    j=input(s17);
    k=input(s18);
    aj=abs(fix(j));ak=abs(fix(k));
    if j~=aj | j>m | k~=ak | k>m | j==0 | k==0
      disp(s13)
      disp(s11)
      pause
      sig='N';
      mess=s0;
    end
    if sig=='Y'
      oldA=A;
      temp=A(j,:);
      A(j,:)=A(k,:);
      A(k,:)=temp;
      mess= [s14 ' Row ' int2str(j) ' ' arrow ' Row ' int2str(k) '.'];
    end
  end
  if ch==2
    sig='Y';
    k=input(s3);
    while (k==0)
      k=input(s4);
    end
    j=input(s5);
    aj=abs(fix(j));
    if j~=aj | j>m | j==0
      disp(s13)    
      disp(s11)
      pause
      sig='N';
      mess=s0;
    end
    if sig=='Y'
      oldA=A;
      A(j,:)=k*A(j,:);
      mess=[s15 num2str(k) ' * Row ' int2str(j) '.'];
    end
  end
  if ch==3
    sig='Y';
    disp(s19)
    disp(s20)
    disp(s21)
    disp(s22)
    disp(s23)
    t=input(s3);
    k=input(s1);
    j=input(s6);
    aj=abs(fix(j));ak=abs(fix(k));
    if j~=aj | j>m | k~=ak | k>m | k==0 | j==0
      disp(s13)
      disp(s11)
      pause
      sig='N';
      mess=s0;
    end
    if sig=='Y'
      oldA=A;
% Change elementary row operation to: Row j --> Row j - mult*Row k
      A(j,:)=-t*A(k,:)+A(j,:);
% Originally: Row j --> Row j + mult*Row k
%      A(j,:)=-t*A(k,:)+A(j,:);
      for ii=1:n,if abs(A(j,ii))<=myeps,A(j,ii)=0;end,end
      mess=[s16 '-' num2str(t) ' * Row ' int2str(k) ' + Row '];
      mess=[mess int2str(j) '.'];
    end
  end
end
AEQ=A;
