module stats;
import std.utf;
import std.stdio;
import std.math;
import std.file;
import std.algorithm;
import std.range;

Class Stats{
	double[][] data;
	Private File data_file;
	double[string] metadata;

	

	this(File data_file){
			private string filename;
			Scanf('%s',&filename);
			this.data_file.open(filename,'r');
    }
	this(double[][] data){
			int j=0;
			foreach(uint i,char[] line;lines(data_file)){
				    while(char[j]!=char[$-1]){
							readf('%d',%data[i][j]);
							j++;
                    }
             }
    }	
	this(double[string] metadata){
//need to take into account the multidimensional nature of data	
	   	this.metadata["mean"]=//mean
	   	this.metadata["std"]=	//standard devation
			
	}
	this(){}
	private double mean(data[][],ref double[string] metadata){range((a,b)(return a+b),x,this.data)/this.data.length;}
   	private double  std(){range((a,b)(return a+abs(b-this.metadata[mean]),x,this.data)/this.data.length;}
	double[string] lin_reg(data,ref double[string] metadata){}
	double t_test(data,ref double[string] metadata){}
	
	
	


		   
	