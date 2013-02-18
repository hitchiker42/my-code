#include ...
//Untar and make a program from source
/*Notes to self
tar exensions:bzip2=.bz2=-j   xz=.xz=-xz   gzip=.gz=-z  */
void main(string name){
    dir=name[path:$];//name=archive file name
    path=name[:filename];
    ext=dir[tar:$];
		switch (ext){
		    case 'bz2':
				ext='-j';
				break;
			 case 'xz':
				ext='-xz';
				break;
			 case 'gz':
				ext='-z';
				break;
			 default:
				ext='';
				break;
	}
	system("%s%s tar %s",&path,&dir,&ext);
    dir=name-.tar.ext;
	string[] ls=system("ls %s%s",&path,&dir);
	if (configure in ls)
		system("%s%s./configure",&path,&dir);
    system("%s%s make",&path,&dir)
	if (install in ls)
		system("%s%s make install",&path,&dir)
}


