/**

   String Pool Allocater

   Allocates strings in batches of 1024.

 */

void freeStringPool();
void initStringPool();
char* requestSpace(unsigned int size);
