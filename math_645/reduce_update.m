function AEQ = Reduce(A,r)
         % Cleaned up/modified by Tucker DiNapoli 17/09/16
%REDUCE  Perform row reduction on matrix A by explicitly choosing
%        row operations to use. A row operation can be "undone", but
%        this feature can not be used in succession.
%
%  Use in the form --- > reduce(A,'r')  for a rational display format
%   or in the form --- > reduce(A)   for a decimal display format
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
  function display_matrix(A, r)
    if(r)
      Amat = A
      format rat
      A = Amat,A;
    else
      format long,A
    end%if
    
  if(nargin < 2)
    rsig = 0;
  else
    rsig = 1;
  end%if
  [m,n] = size(A);
  oldA = A;
  myeps = 1e-14; %my tolerance for zero in rational display
  %% Set up strings to be used as messages.
  %% Matlab only supports single quotes, so use those.
  strs = ...
  {'     ',
   'Enter number of the row that you want to multiply by the MULTIPLIER.  ',
   'Enter second row number.  ',
   'Enter MULTIPLIER.  ',
   'Enter nonzero MULTIPLIER.  ',
   'Enter row number.  ',
   'Enter number of row that changes.  ',
   'Last row operation "undone". ',
   '               ***** "REDUCE" a Matrix by Row Reduction *****',
   '         The current matrix is:',
   '     ENTER your choice --- > ',
   'Press  ENTER  to continue',
   '*****  -- > REDUCE is over. Your final matrix is:',
   '** Improper row number! **',
   'Interchange Complete: ',
   'Row Multiplication Complete: ',
   'Replacement by Linear Combination Complete: ',
   'Enter the number of one of the rows that you want to swap. ',
   'Enter the number of the other row that you want to swap. ',
   'For this option you will be asked for a MULTIPLIER which will then ',
   'be multiplied by every element in the row you specify next.  The ',
   'multiple of the row will then be SUBTRACTED from the last row you ',
   'enter (i.e., the row that changes).  The first row you entered ',
   'remains unchanged. '};

  arrow = [char(60) char(196) char(62)];
  menu = ...
  ['            OPTIONS             ',
   ' < 1>  Interchange two rows.',
   ' < 2>  Multiply a row by a nonzero scalar and display.',
   ' < 3>  Replace:  row i by (row i - mult*row j)',
   ' < 4>  Turn on rational',
   ' < 5>  Turn off rational',
   ' < 6>  Display current matrix.',
   ' <-1> "Undo" previous row operation and display.',
   ' < 0>  Quit reduce!'];
  mess = strs{9};
  i = sqrt(-1); 
  while(1)
    %%cla ?
    disp(mess)
    disp(strs{10})
    a = A; %to prevent case mismatch in multiplier choice
         %  if rsig == 'T',Amat = A;A = rat(A,'s'),A = Amat;else,A,end

    disp(menu)
    ch = input(strs{11});
    switch(ch)
      case -1 %undo last operation
        A = oldA;
        mess = strs{8};
      case 0 %quit
             %cla
        disp(strs{13})
        disp(strs{1})
        %%    if rsig == 'T',Amat = A;A = rat(A,'s'),A = Amat;else,A,end
        if(rsig)
          Amat = A;format rat,A = Amat,A
        else
          format long,A
        end%if
        AEQ = A;
        break
      case 4
        rsig = 1
      case 5
        rsig = 0
      case 6
        disp(a)
    end%if
    if ch == 1
      sig = 'Y';
      j = input(strs{18});
      k = input(strs{19});
      aj = abs(fix(j));ak = abs(fix(k));
      if(j ~= aj | j > m | k ~= ak | k > m | j == 0 | k == 0)
        disp(strs{14})
        disp(strs{12})
        pause
        sig = 'N';
        mess = strs{1};
      end%if
      if(sig == 'Y')
        oldA = A;
        temp = A(j,:);
        A(j,:) = A(k,:);
        A(k,:) = temp;
        mess = [strs{15} ' Row ' int2str(j) ' ' arrow ' Row ' int2str(k) '.'];
      end%if
    end%if
    if(ch == 2)
      sig = 'Y';
      k = input(strs{4});
      while (k == 0)
        k = input(strs{5});
      end%while
      j = input(strs{6});
      aj = abs(fix(j));
      if(j ~= aj | j > m | j == 0)
        disp(strs{14})    
        disp(strs{12})
        pause
        sig = 'N';
        mess = strs{1};
      end%if
      if(sig == 'Y')
        oldA = A;
        A(j,:) = k*A(j,:);
        mess = [strs{16} num2str(k) ' * Row ' int2str(j) '.'];
      end%if
    end%if
    if(ch == 3)
      sig = 'Y';
      disp(strs{20})
      disp(strs{21})
      disp(strs{22})
      disp(strs{23})
      disp(strs{24})
      t = input(strs{4});
      k = input(strs{2});
      j = input(strs{7});
      aj = abs(fix(j));ak = abs(fix(k));
      if(j ~= aj | j > m | k ~= ak | k > m | k == 0 | j == 0)
        disp(strs{14})
        disp(strs{12})
        pause
        sig = 'N';
        mess = strs{1};
      end%if
      if(sig == 'Y')
        oldA = A;
             % Change elementary row operation to: Row j -- > Row j - mult*Row k
        A(j,:) = -t*A(k,:)+A(j,:);
                                % Originally: Row j -- > Row j + mult*Row k
                                %      A(j,:) = -t*A(k,:)+A(j,:);
        for ii = 1:n
          if(abs(A(j,ii)) <= myeps)
            A(j,ii) = 0;
          end%if
        end%for
        mess = [strs{17} '-' num2str(t) ' * Row ' int2str(k) ' + Row '];
        mess = [mess int2str(j) '.'];
      end%if
    end%if
  end%while
  AEQ = A;
