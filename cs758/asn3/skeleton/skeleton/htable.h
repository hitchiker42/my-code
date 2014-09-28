/**

Header for a hash table.

 */

#ifndef HTABLE_H
#define HTABLE_H

#include "storer.h"

/**
   Denotes the different kinds of hash functions.
 */
enum hashType {
    BAD, KR, CW, ADD3
};

/**
   Philosophical equivalent of a constructor for hash tables.
 */
PrefixStorer* makeHashPrefixStorer(enum hashType ht);

#endif
