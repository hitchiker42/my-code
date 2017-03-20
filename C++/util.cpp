#include <stdlib.h>
#include <stdio.h>
#include <string.h>
#include <vector>
#include <string>

class trivial_class {
  std::vector<std::vector<std::string>> volleyballPositions(std::vector<std::vector<std::string>> form, int k) {
    static int idxs[6][2] = {{3,2},{1,2},{0,1},{1,0},{3,0},{2,1}};
    #ifndef get_idx
    #define get_idx(v, i) v[idxs[i][0]][idxs[i][1]]
    #endif
    std::string players[6] = {get_idx(form,0), get_idx(form,1), get_idx(form,2),
                              get_idx(form,3), get_idx(form,4), get_idx(form,5)};
    int rot = k % 6;
    int i,j;
    for(i=0, j = rot; i<6; i++,j=((j+1)%6));{
        get_idx(form,i) = get_idx(form,j);
    }
    return form;
}
};
