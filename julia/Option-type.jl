module Option_type
type Some{T}
    x::T
end
type None{T}
end
Option{T}=Union(Some{T},None{T})
