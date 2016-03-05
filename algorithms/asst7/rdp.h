
/**
 This function is the core of the recursive descent parser.  It should produces a
 parse that looks like this:
 
 LHS(RHS1 RHS2)
 
 This is recursive, so you can have stuff like this:
 
 S(NP(N(fish) N(fish)) VP(V(fish) NP(fish)))
 
 */


#ifndef Parser_rdp_h
#define Parser_rdp_h

#define RDP_MAX 20

void rdp_parse(void);

#endif
