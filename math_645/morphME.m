% This script will morph a square into an octagon. 
%(add more points to make it look more like a circle).



p1=[0;0]; p2=[0.5;0]; p3=[1;0]; p4=[1;0.5]; p5=[1;1]; p6=[0.5;1]; p7=[0;1]; p8=[0;0.5];p9=[0;0];

B = [p1,p2,p3,p4,p5,p6,p7,p8,p9];  %B is the "before" view, i.e., a square
fill(B(1,:),B(2,:),'r')
axis([-2 2 -2 2])
hold on

%now define the "after", a circle-ish shape
a1=[-1/sqrt(2);-1/sqrt(2)]; a2=[0;-1]; a3=[1/sqrt(2);-1/sqrt(2)]; a4=[1;0]; a5=[1/sqrt(2);1/sqrt(2)]; a6=[0;1]; a7=[-1/sqrt(2);1/sqrt(2)]; 
a8=[-1;0];a9=[-1/sqrt(2);-1/sqrt(2)];

A=[a1,a2,a3,a4,a5,a6,a7,a8,a9];

color = str2mat('y','m','c','r','g','b','k');

for k=0:1/6:1

	C = (1-k)*B + k*A;

	fill(C(1,:),C(2,:),color(k*6+1))

	pause(0.2);

end

hold off