#include <guile/2.0/libguile.h>
#include <openssl/md4.h>
SCM
MD4_wrapper (SCM scm_msg)
{
  /* The call to scm_bytevector_length should do the typechecking for me */
  unsigned long msg_len = scm_c_bytevector_length (scm_msg);
  /* This might not be the fastest way to do this, but it should be safe */
  SCM scm_digest = scm_c_make_bytevector(MD4_DIGEST_LENGTH);
  unsigned char *digest = SCM_BYTEVECTOR_CONTENTS(scm_digest);
  unsigned char *msg = SCM_BYTEVECTOR_CONTENTS(scm_msg);

  MD4(msg, msg_len, digest);

  return scm_digest;
}
void
init_MD4 ()
{
  scm_c_define_gsubr("MD4",1,0,0,MD4_wrapper);
}


/* Local Variables: */
/* eval: (c-set-style "gnu") */
/* End: */
