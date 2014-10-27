module ArrayPair
typealias Pair{T} (T,T)
typealias ArrayPair{T} Array{(T,T)}
export zip,map2,unzip
zip(a::Array{T},b::Array{T})=[(a[i],b[i]) for i=1:length(a)]::ArrayPair{T}