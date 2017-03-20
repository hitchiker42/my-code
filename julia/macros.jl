module Util
#convert(t::Type{Bool}, x::
macro aif(cond,t,e)
  quote
    let $(esc(:it)) = $(esc(cond))
      if convert(Bool,$(esc(:it)))
        $(esc(t))
      else
        $(esc(e))
      end
    end
  end
end
end
