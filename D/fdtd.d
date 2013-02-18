import std.math,std.parallelism,std.conv;
immutable double c=299792458;
immutable double e_0=8.85418781762039e-12;
immutable double u_0=(4*PI)e-7;
/*   .___.~
 *  /|  /|~
 * /_|_/ |~
 * |/  | /~
 * |___|/ ~
 *look an ascii art box woo!, it's 5 by 7
 *We have a grid lets say its 3x3x3, (0,0,0) is the bottom left corner
 *at (0,0/2,1)and (2,0/2,1) are Ex nodes
 *at (1,0/2,0) and (1,0/2,2) are Ey
 *at (2,1,0/2) and (2,1,0/2) are Ez nodes
 *so E nodes at the middle of the intersection of each side
 *we have Hx at(1,1,0/2) Hy at(0/2,1,1) and Hz at (1,0/2,1)
 *so H's at the center of each face
 *the unit used is Del of x,y,z respectively
 */
int main(string[] args){
	dt=to!double(args[1]);
	dx=to!double(args[2]);//assume dy&dz have same step
	double[][] Ex,Ey,Ez;//form should be Ex=[[Exx@t0,Exy@t0,Exz@t0],[Exx@t1,Exy@t1,Exz@t1]...[Exx@tn,Exy@tn,Exz@tn]] with Ex replaced with whatever field
	double[][] Hx,Hy,Hz;
	auto E=[Ex,Ey,Ez];
	auto H=[Hx,Hy,Hz];
	for(i=0,i<n,i++){}}

double[][][] FDTDH(sigma,mu,epsilon,dt,dr,H[],E[]){//update step maybe
//Each componant of E is a tuple with the first value being closest to the bottom left
	sigma=0;mu=u;epsilon=e;//lets assume all these are the same in each dimension
	Hu=(u-0.5*dt*o)/(u+0.5*dt*o);
	Eu=(e-0.5*dt*o)/(e+0.5*dt*o);
	auto dx,dy,dz;
	dx=dy=dz=dr;
	Ht=dt/(u+0.5*dt*o);
	Et=dt/(e+0.5*dt*o);
	double[3][2] Hn,En;
	Hn[0]=Hu*H[0]+Ht*(((E[1][1]-E[1][0])/dz)-((E[2][1]-E[2][0])/dy));
	Hn[1]=Hu*H[1]+Ht*(((E[2][1]-E[2][0])/dx)-((E[0][1]-E[0][0])/dz));
	Hn[2]=Hu*H[2]+Ht*(((E[0][1]-E[0][0])/dy)-((E[1][1]-E[1][0])/dx));
	En[0]=Eu*E[0][0]+Et*(((Hn[1]-H[1])/dz)-((Hn[2]-H[2])/dy));
	En[1]=Eu*E[1][0]+Et*(((Hn[2]-H[2])/dx)-((Hn[0]-H[0])/dz));
	En[2]=Eu*E[2][0]+Et*(((Hn[0]-H[0])/dy)-((Hn[1]-H[1])/dx));
//I am So Confused right now