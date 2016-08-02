#include <stdio.h>
#include <pthread.h>
#include <semaphore.h>

#define print_sizeof(type)                      \
  printf("sizeof(%s) = %d\n", #type, sizeof(type));
int main(){
  print_sizeof(pthread_t);
  print_sizeof(pthread_attr_t);
  print_sizeof(sem_t);
  print_sizeof(pthread_mutex_t);
  print_sizeof(pthread_key_t);
  print_sizeof(pthread_rwlock_t);
  print_sizeof(pthread_rwlockattr_t);
  print_sizeof(pthread_cond_t);
  print_sizeof(pthread_condattr_t);
  print_sizeof(pthread_t);
}
