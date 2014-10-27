module Units
using Match
abstract SI
abstract unit
abstract SIbase<:SI
immutable meter <: SIbase end#m
immutable kilogram <: SIbase end#kg
immutable second <: SIbase end#s
immutable ampere <: SIbase end#A
immutable kelvin <: SIbase end#K
immutable candela <: SIbase end#cd
immutable mole <: SIbase end#Mol
abstract SIderived<:SI
immutable celsius <: SIderived end#C, = K
immutable tesla <: SIderived end#T, = kg/s^2*A
immutable ohm <: SIderived end#Ω, = kg*m^2/s^3*A^2
immutable volt <: SIderived end#V, = kg*m^2/s^3*A
immutable coulomb <: SIderived end#C, = s*A
immutable watt <: SIderived end#W, = kg*m^2/s^3
immutable joule <: SIderived end#J, = kg*m^2/s^2
immutable pascal <: SIderived end#Pa, = kg/m*s^2
immutable newton <: SIderived end#N, = kg*m/s^2
immutable radian <: SIderived end#rad, = m/m
immutable hertz <: SIderived end#Hz, = 1/s
abstract SIPrefix
immutable Yocto <: SIPrefix end
immutable Zepto <: SIPrefix end
immutable Atto <: SIPrefix end
immutable Femto <: SIPrefix end
immutable Pico <: SIPrefix end
immutable Nano <: SIPrefix end
immutable Micro <: SIPrefix end
immutable Milli <: SIPrefix end
immutable Centi <: SIPrefix end
immutable Deci <: SIPrefix end
immutable SINone <: SIPrefix end
immutable Deca <: SIPrefix end
immutable Hecto <: SIPrefix end
immutable Kilo <: SIPrefix end
immutable Mega <: SIPrefix end
immutable Giga <: SIPrefix end
immutable Tera <: SIPrefix end
immutable Peta <: SIPrefix end
immutable Exa <: SIPrefix end
immutable Zetta <: SIPrefix end
immutable Yotta <: SIPrefix end
immutable SIComposite <: SIderived
    meter :: Integer
    kilogram :: Integer
    second :: Integer
    ampere :: Integer
    kelvin :: Integer
    candela :: Integer
    mole :: Integer
end
immutable Value{T}
    unit::SI
    val::T
end
function toString(unit::SI)
    @Match unit begin
        meter => "m"
        kilogram => "kg"
        second => "s"
        ampere => "A"
        kelvin => "K"
        candela => "cd"
        mole => "Mol"
        celsius => "℃" 
        tesla => "T"
        ohm => "Ω"
        volt => "V"
        coulomb => "C"
        watt => "W"
        joule => "J"
        pascal => "Pa"
        newton => "N"
        radian => "rad"
        hertz => "Hz"
    end
end
    