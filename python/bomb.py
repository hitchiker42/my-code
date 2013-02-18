import numpy as np
import scipy as sci
import matplotlib as mpl
from matplotlib import pyplot as plt
import uncertainties as unc
from uncertainties import ufloat,unumpy
T_ba=unumpy.uarray[0(1),1(1)];
mmba=ufloat(110);
mba=ufloat(0);
n_ba=mba/mmba;
del_H_ba=ufloat(0);
R=ufloat();
del_n_ba=ufloat(-0.5);
qba=n_ba*(del_H_ba-(del_n_ba*R*T_ba[0]));
qwire=ufloat(1(0));
delT_ba=T_ba[1]-T_ba[0];
Cal=((-qba)+(-qwire))/delT_ba;

T_camp=unumpy.uarray[0(1),0(1)];
q_camp=-(Cal*(T_camp[1]-T_camp[0])+qwire);
mmcamp=ufloat();
mcamp=ufloat();
n_camp=mcamp/mmcamp;
del_n_camp=ufloat(-3.5);
del_H_camp=(q_camp/n_camp)+(del_n_camp*R*T_camp[0]);

T_chip=unumpy.uarray[0(1),1(1)];
q_chip=-(Cal*(T_chip[1]-T_chip[0])+qwire)

T_light=unumpy.uarray[0(1),1(1)]
q_light=-(Cal*(T_light[1]-T_ligth[0])+qwire)
