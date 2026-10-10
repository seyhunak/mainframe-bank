      *> ====================================================================
      *> LOCKREC - Record locking dataset             DSORG=Indexed KSDS
      *> GnuCOBOL has no ENQ/DEQ, so the lock the engine takes on an account
      *> before a posting is a record in its own dataset: a second writer
      *> gets a duplicate key (status 22) and is told the account is in use.
      *> A lock carries the time it was taken so a stale one can be broken
      *> by the night-end sweep after LOCK-MAX-MIN minutes.
      *> ====================================================================
       01  LOCK-REC.
           05  LK-KEY          PIC X(12).
           05  LK-TERM         PIC X(4).
           05  LK-USER         PIC X(8).
           05  LK-DATE         PIC 9(8).
           05  LK-TIME         PIC 9(6).
           05  FILLER          PIC X(6).
