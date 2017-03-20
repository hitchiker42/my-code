module HW1
using constants
export two_a
let r=0.0529,e=e_charge
    function two_a()
        *(/(1,*(4pi*episilon_0,r)),+(+(*(-e,4e/3),*(-e,3e/2)),*(e,e/2)))
        end
    end
end
