% This script will move the letter N about the screen using
% homogeneous coordinate transformations common to computer graphics

p1=[0;0];
p2=[0.5;0];
p3=[6;0];
p4=[5.5;1.58];
p5=[0.5;6.42];
p6=[0;8];
p7=[5.5;8];
p8=[6;8];

D = [p1,p6,p4,p7,p8,p3,p5,p2]
fill(D(1,:),D(2,:),'r')
axis([-30 30 -30 30])
hold on

N = [D;ones(1,8)]
N1=N;

R = [cos(pi/18) -sin(pi/18) 0; sin(pi/18) cos(pi/18) 0; 0 0 1]

color = str2mat('y','m','c','r','g','b','k');

for i=0:5

	Shift = [1 0 i; 0 1 i; 0 0 1];

	N1 = Shift*N1;

for j=1:36

	N1 = R*N1;

	D1 = N1(1:2,:);

	fill(D1(1,:),D1(2,:),color(rem(j,7)+1))

	pause(0.15);

end
end


hold off
