function AEQ = Reduce(A,r)
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
%  Reformated/Cleaned up by Tucker DiNapoli on September 17, 2016
%
%  Usage: --when prompted for first row: enter row number j
%         --when prompted for row that changes: enter row number i
%
  function display_matrix(A, r)
    if(r)
      format rat, A
    else
      format, A
    end%if
  end%function

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
   ['For this option you will be asked for a MULTIPLIER which will then ',
    'be multiplied by every element in the row you specify next.  The ',
    'multiple of the row will then be SUBTRACTED from the last row you ',
    'enter (i.e., the row that changes).  The first row you entered ',
    'remains unchanged. ']};

  arrow = [char(60) char(196) char(62)];
  menu = ...
  ['            OPTIONS             ',
   ' < 1>  Interchange two rows.',
   ' < 2>  Multiply a row by a nonzero scalar.',
   ' < 3>  Replace:  A(i,:) = A(i,:) - c * A(j,:)',
   ' < 4>  Turn on rational display',
   ' < 5>  Turn off rational display',
   ' < 6>  Display current matrix.',
   ' <-1> "Undo" previous row operation.',
   ' < 0>  Quit reduce!'];
  current_message = strs{9};

  while(1)
    %%cla ?
    disp(current_message)
    disp(strs{10})
    a = A; %to prevent case mismatch in multiplier choice
    display_matrix(A, rsig)
    disp(menu)
    ch = input(strs{11});
    switch(ch)
      case -1 %undo last operation
        A = oldA;
        current_message = strs{8};
      case 0 %quit
        disp(strs{13})
        disp(strs{1})
        display_matrix(A, rsig)
        AEQ = A;
        break
      case 1 %Swap rows
        i = input(strs{18});
        j = input(strs{19});
        ai = abs(fix(i));
        aj = abs(fix(j));
        %% |/& magically short circut when used in if/while statements
        if(i ~= ai | i > m | j ~= aj | j > m | i == 0 | j == 0)
          %% Invalid input
          disp(strs{14})
          disp(strs{12})
          pause
          current_message = strs{1};
        else 
          oldA = A;
          temp = A(i,:);
          A(i,:) = A(j,:);
          A(j,:) = temp;
          current_message = [strs{15}, ' Row ', int2str(i), ...
                             ' ', arrow, ' Row ', int2str(j) '.'];
        end%if
      case 2 %Multiply by scalar
        c = input(strs{4});
        while (c == 0)
          c = input(strs{5});
        end%while
        i = input(strs{6});
        ai = abs(fix(i));
        if(i ~= ai | i > m | i == 0)
          %%Invalid input
          disp(strs{14})
          disp(strs{12})
          pause
          current_message = strs{1};
        else
          oldA = A;
          A(i,:) = c*A(i,:);
          current_message = [strs{16}, num2str(c), ' * Row ', int2str(i), '.'];
        end%if
      case 3 %A[i,:] = A[i,:] - c*A[j,:]
        disp(strs{20})
        c = input(strs{4});
        j = input(strs{2});
        i = input(strs{7});
        ai = abs(fix(i));
        aj = abs(fix(j));
        if(i ~= ai | i > m | j ~= aj | j > m | j == 0 | i == 0)
          disp(strs{14})
          disp(strs{12})
          pause
          current_message = strs{1};
        else
          oldA = A;
          A(i,:) = A(i,:)-c*A(j,:);
          %% Originally: Row j -- > Row j + mult*Row k
          %%      A(j,:) = -t*A(k,:)+A(j,:);
          
          %% Originally a for loop, which is bad style for matlab code
          A = A .* (A>myeps);
          current_message = [strs{17}, '-', num2str(c), ' * Row ', ...
                             int2str(j) ' + Row '];
          current_message = [current_message, int2str(i), '.'];
        end%if
    case 4
      rsig = 1;
    case 5
      rsig = 0;
    case 6
      %% set the current_messageage to blanks and loop, the matrix will be printed
      current_message = strs{1};
    end%switch
  end%while
  AEQ = A;
end%function
