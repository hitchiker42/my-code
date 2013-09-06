module constants
export h_bar,mu_0,episilon_0,G,h,c,e_charge,m_e,m_p,amu,k_b,N_a,R,e_volt
const c = 299_792_458.0 #speed of light, m/s
const G = 6.673_84e-11 #constant of gravitation m^3/kg*s^2
const h = 6.626_069_57e-35 #plank constant j*s
const h_bar = h/2pi
const episilon_0 = 8.854_187e-12 #vaccum permittivity F/m
const mu_0 = 4pi*1e-7 #vaccumm permeability N/A^2
const e_charge = 1.602_176_565e-19#elementary charge C
const m_e = 9.109_382_91e-28 #electron mass g
const m_p = 1.672_621_777e-24 #proton mass g
const amu = 1.660_538_921e-24 #atomic mass unit g
const k_b = 1.380_6488 #boltzman constant J/K
const N_a = 6.022_141_29e23 #avogadro's number 1/mol
const R = k_b*N_a #Ideal gas constant J/K*mol
const e_volt = 1.602_176_565e-19#electron volt in joules
end

