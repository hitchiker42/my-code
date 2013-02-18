import std.math,core.vararg,std.traits;
int main(){
double[][] Rk4(double dt,double[] y,double function(double t,double y) f[],...){
	int i,j,n=1;
	double s1,s2,s3,s4,t=0;
	for (i=0;i<_arguments.length;i++){
		if (_arguments[i] == typeid(int)){
			n=va_arg!(int)(_argptr);
		}
		else if (_arguments[i]==typeid(double)){
			t=va_arg!(double)(_argptr);
		}
	}
	double[][] yf;
	yf[0]=y;
	for(i=0;i<n-1;i++){
		for(j=0;j<y.length;j++){
			s1=dt*f[i](t,yf[i][j]);
			s2=dt*f[i](t+(dt/2),yf[i][j]+(s1/2));
			s3=dt*f[i](t+(dt/2),yf[i][j]+(s2/2));
			s4=dt*f[i](t+dt,yf[i][j]+s3);
			yf[i+i][j]=(yf[i][j]+((dt/6)*(s1+2*s2+2*s3+s4)));
		}
		t+=dt;
	}
	return(yf);
}
return(0);
}