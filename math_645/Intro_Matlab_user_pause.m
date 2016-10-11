%This MATLAB script will give you an introduction to MATLAB's capabilities.
%CAUTION: When you are typing in lines interactively, you should ignore lines
%that start with the % symbol, since those are just comments.  Also, the 
%pause commands should be skipped as well as the disp (display) commands.
%In the future, you should remember that if you put a semicolon (;) at the
%end of a line, MATLAB doesn't print out to the screen - this is very useful
%if you are defining a vector with 100,000 elements.  When you are done
%with MATLAB, just type quit and you will exit.  The best place to look for
%information on a given MATLAB function is in the Reference Guide, and if
%you want to learn more about the overall operation of MATLAB, read the 
%Tutorial in Chapter 2 of the User's Guide.

disp('Let''s define a vector of time values with the command')
disp('>> time = 1:10')
pause

time = 1:10

pause

disp('In the previous command, we gave it no step size , so MATLAB assumed')
disp('we wanted an increment of 1 between time values. Now let''s take')
disp('time values between 1 and 10, with step=0.25 using the command')
disp('>> time = 1:0.25:10')
pause

time = 1:0.25:10

pause

disp('For each time value, we can calculate the sine of the time value')
disp('using the single command')
disp('>> y = sin(time)')
pause

y = sin(time)

pause

disp('Notice that y is automatically created as a vector.  Now, to ')
disp('create a plot of y vs. time, use the command')
disp('>> plot(time,y)')
disp('An image window will soon open up on your screen.  If it is blocking')
disp('your view, use the mouse to move the cursor to the top bar on the image')
disp('window, hold the left button down and drag it out of the way.')
pause

plot(time,y)

pause

disp('To change the scale on the axes use the command')
disp('>> axis([1 10 -1.5 1.5])')
pause

axis([1 10 -1.5 1.5])

pause

disp('We can put labels on the axes with the commands')
disp('>> xlabel(''t'')')
disp('>> ylabel(''y(t)'')')
pause

xlabel('t')
ylabel('y(t)')

pause

disp('Now for a title using')
disp('>> title(''sin(t) vs. t'')')
pause

title('sin(t) vs. t')

pause

disp('Now to clear the image window, type')
disp('>> clf')
pause

clf

pause
disp(' ')
disp(' ')
disp('Now create a matrix which has sin(time) in the first column and')
disp('cos(time) in the second column.  First try the command')
disp('>> z = [sin(time) cos(time)]')
pause

z = [sin(time) cos(time)]

pause

disp('Notice that we did not get two columns for z, all we got was one')
disp('long vector.  Well, that makes sense, since sin(time) and cos(time)')
disp('are just row vectors and we put them side by side.  We need to use')
disp('the transpose to make them column vectors.  To take the transpose of')
disp('a matrix or vector, use the single quote character,('').  So try')
disp('>> z = [sin(time)'' cos(time)'']')
pause

z = [sin(time)' cos(time)']

pause

disp('To plot the two columns of z against time, type')
disp('>> plot(time,z)')
pause

plot(time,z)

pause

disp('Since z is a matrix, you might want to look at (or use) just one element')
disp('and since the elements are labeled z(row,col), the command')
disp('>> z(1,1)')
disp('gives the element in the first row and the first column.')
pause

z(1,1)

pause

disp('Similarly, the element in the first row, second column is given by')
disp('>> z(1,2)')

pause

z(1,2)

pause

disp('Now, to get the entire first column')
disp('>> z(:,1)')
pause

z(:,1)

pause

disp('And, for the second column')
disp('>> z(:,2)')
pause

z(:,2)

pause

disp('Now let''s plot the first column versus the second column')
disp('>> plot(z(:,1),z(:,2))')
pause

plot(z(:,1),z(:,2))

pause

disp('To clear image window')
disp('>> clf')
pause

clf

pause

disp(' ')
disp(' ')
disp(' ')
disp('Now for a more complicated graph.  Define f to be the function')
disp('>> f = sin(2*pi*time)./time;')
disp('where the operator ./ means to take every element in the vector')
disp(' sin(2*pi*time) and divide it by the associated element in time.')
pause

f = sin(2*pi*time)./time

pause

disp('Now plot f vs. time')
disp('>> plot(time,f)')
pause

plot(time,f)

pause

disp('The jagged peaks don''t make sense for our smooth function, so we')
disp('need to take finer time increments.  So we redefine time with step=0.1')
disp('>> time = 0.1:0.1:10;')
disp('and notice the semicolon (;) at the end of the command which tells')
disp('Matlab not to print out the vector (since it has 100 elements)')
pause

time = 0.1:0.1:10;



disp('Redefine f')
disp('>> f = sin(2*pi*time)./time;')
pause

f = sin(2*pi*time)./time;

disp('We can check a few entries in f by typing')
disp('>> f(1:10)')
disp('so we can check if the changes are smaller between points.')
pause

f(1:10)

pause

disp('Now plot f vs. time')
disp('>> plot(time,f)')
pause

plot(time,f)

pause

disp('Now look at matrix operations.  Define a matrix with (;) separating rows')
disp('>> A = [ 1 2; 3 4]')
pause

A = [ 1 2; 3 4]

pause

disp('Define a column vector')
disp('>> v = [1; 1]')
pause

v = [1; 1]

pause

disp('Define a new vector, w, to be the product Av')
disp('>> w = A*v')
pause

w = A*v

pause

disp('Define a new matrix, B, to have v in the first col and w in the second')
disp('>> B = [v w]')
pause

B = [v w]

pause

disp('Let C be the product AB')
disp('>> C = A*B')
pause

C = A*B

pause

disp('Let D be the product BA (remember matrix mult is non-commutative)')
disp('>> D = B*A')
pause

D = B*A

pause

disp('Redefine B to be a 2 x 3 matrix, with the last column = v + w')
disp('>> B = [ v w v+w]')
pause

B = [ v w v+w]

pause

disp('calculate the product')
disp('>> C = A*B')

pause

C = A*B

pause

disp('When doing this on your own, try the product BA')
disp('>> D = B*A')
disp('If you do, you will get an error message; since B is 2 x 3 and A is 2 x 2,')
disp('the product BA is not defined.  However, the transpose of B is given ')
disp('by B'' and it is 3 x 2, so let''s try')
disp('>> D = B''*A')
pause

D = B'*A

pause

disp('Now, since we know that A*v=w, if we solve the system A*x=w')
disp('for x unknown, we should get that x=w.  This is such a common')
disp('problem in applications, MATLAB uses the \ symbol to indicate')
disp('that you want to solve for x.  This is done with the command')
disp('>> x = A\w')
pause

x = A\w

pause

disp('and we see that x=v as expected.')
disp('Note: If you transposed everything so that we had x''*A''=w'', then')
disp('MATLAB would use the symbol / to find x''=w''/A'' similarly.')
pause
disp('Now, try out MATLAB''s eigenvector/eigenvalue routine.  The command')
disp('>> [V,D]=eig(A)')
disp('calculates the eigenvectors of A and puts them in columns of V,')
disp('and puts the eigenvalues on the diagonal of D.')
pause

[V,D]=eig(A)

pause

disp('Remember that A multiplied by an eigenvector should give back an')
disp('eigenvalue times the eigenvector.  Since the eigenvectors are in the')
disp('columns of V, the product AV gives us A times both eigenvectors at once.')
disp('>> E=A*V')
pause

E=A*V

pause
disp('Let''s check the result by taking the first eigenvalue and multiplying')
disp('it by the first column of V to make sure it equals the first col of E.')
disp('>> D(1,1)*V(:,1)')
pause

D(1,1)*V(:,1)

pause

disp('and for the second eigenvalue and second column')
disp('>> D(2,2)*V(:,2)')
pause

D(2,2)*V(:,2)


disp('So each column of E is just an eigenvector times its eigenvalue')
pause

disp('Finally, there are a few MATLAB functions which are convenient for')
disp('initializing large matrices or defining certain matrices automatically.')
disp('To define a 4 x 5 matrix with random entries, type')
disp('>> M = rand(4,5)')
pause

M = rand(4,5)

pause
disp('If you want to extract the first three rows and first 2 cols of M, type')
disp('>> N= M(1:3,1:2)')
pause

N=M(1:3,1:2)

pause
disp('An extremely useful function for initialization and space-saving is')
disp('>> N=zeros(2,6)')
pause

N=zeros(2,6)

pause

disp('Or, for a row vector')
disp('>> N = zeros(1,6)')
pause

N = zeros(1,6)

pause

disp('Similarly, we can define a 2 x 3 matrix of 1s')
disp('>> N = ones(2,3)')
pause

N = ones(2,3)

pause

disp('The 3 x 3 identity matrix is given by (note the name!!)')
disp('>> N = eye(3)')
pause(8)

N = eye(3)

pause

disp('If you define the matrix to be 3 x 5, the ones are on the main diagonal.')
disp('>> N = eye(3,5)')
pause

N = eye(3,5)

pause
disp('In this tutorial, you have now seen how to enter matrices and vectors,')
disp('calculate matrix and vector products, as well as MATLAB''s special')
disp('element-by-element operations (the ones like .*, .+, etc.).  I hope')
disp('this helps, and remember to look through the MANUALS for more info.')
disp('To learn more about the use of function files and scripts, look at')
disp('the files odesolve1.m, odeinput.m, and vanderpol.m as well as the')
disp('reference material in Ch. 2 of the User''s Guide starting on p.2-128.')
disp('Finally, to get out of MATLAB just type: >> quit')
