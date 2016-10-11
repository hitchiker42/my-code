function PlaneTransform2(A,NumIters)
% f is a function that given an integer should return a 2x2 matrix
%USAGE: [Aout]=PlaneTransform(A,NumIters)

color = {'b.','r.','g.','m.','c.','y.','k.'};
cc1 = {'b *','r *','g *','m *','c *','y *','k *'};
cc2 = {'b o','r o','g o','m o','c o','y o','k o'};

% Creating unit vectors
figure;

it=0;
NN=64;
if it == 0
	
  V=zeros(2,NN);
  V = [cos(2*pi*[0:NN-1]/NN); sin(2*pi*[0:NN-1]/NN)];
  Vorig=V;

end

plot(V(1,:),V(2,:),'b.')

hold on
plot(V(1,1),V(2,1),'b*')
plot(V(1,NN/4+1),V(2,NN/4+1),'bo')
title('Original Vectors - notice the circled point and the starred point')
for kk=1:NumIters
  sleep(0.5)
%  V=f(kk)*V;
  V = A*V
  plot(V(1,:),V(2,:),color{rem(kk,7)+1})
  plot(V(1,1),V(2,1),cc1{rem(kk,7)+1})
  plot(V(1,NN/4+1),V(2,NN/4+1),cc2{rem(kk,7)+1})


axis('equal')
fprintf('Hit any key to continue.\n')
end
hold off
close()
