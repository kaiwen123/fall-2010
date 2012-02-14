
/* wbadmin.x */

program WhiteBoardAdmin {
  version WhiteBoardAdminVersion {
    void callbackfromwbadmin() = 1;
  } = 1;
} = 0x20000999;			/* replaced by gettransient() value */

/* -eof- */
