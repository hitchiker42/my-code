% This script will move the letter N about the screen using
% homogeneous coordinate transformations common to computer graphics
function out = moveOtto(PP);
%p1=[0;0]; p2=[0.5;0]; p3=[6;0]; p4=[5.5;1.58]; p5=[0.5;6.42]; 
%p6=[0;8]; p7=[5.5;8]; p8=[6;8];

%Initial Test of plot
[Nrows,Ncols]=size(PP);
if Nrows ~= 2
   if Ncols ==2
		PP=PP';
   else
		fprintf('ERROR: Input file should have 2-vectors for the input points.\n');
   end
end

plot(PP(1,:),PP(2,:),'.')
title('These are the original points in the image');
clf;

%D = [p1,p6,p4,p7,p8,p3,p5,p2]
%fill(D(1,:),D(2,:),'r')
FigScale=400;
axis([-FigScale FigScale -FigScale FigScale])
hold on

N = [3*PP;ones(1,Ncols)];
N1=N;

R = [cos(pi/8) -sin(pi/8) 0; sin(pi/8) cos(pi/8) 0; 0 0 1];

color = str2mat('y','m','c','r','g','b','k');

disp('Hit any key to start the animation');
pause;

for ii=0:5

	Shift = [1 0 ii; 0 1 ii; 0 0 1];
      SCmatrix = [ 1-ii/8 0 0; 0 1-ii/8 0; 0 0 1];
	N1 = Shift*SCmatrix*N1;

for jj=1:16

	N1 = R*N1;

	PP1 = N1(1:2,:);
      plot(PP1(1,:),PP1(2,:),'.','color',color(rem(jj,7)+1))
	%fill(D1(1,:),D1(2,:),color(rem(jj,7)+1))

	pause(0.01);

	plot(PP1(1,:),PP1(2,:),'.','color','w') %Erase the plot by overwriting as white

end

clf;
axis([-FigScale FigScale -FigScale FigScale])
hold on;
end


hold off
out = 1;
