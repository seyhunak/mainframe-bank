      *> ====================================================================
      *> GLBREC - General ledger balances by control account    KSDS=Indexed
      *> Maintained as entries are written so the night-end proof reads one
      *> dataset instead of the whole ledger.
      *> ====================================================================
       01  GLB-REC.
           05  GB-ACCT         PIC X(8).
           05  GB-NAME         PIC X(30).
           05  GB-DEBITS       PIC S9(13)V99 COMP-3.
           05  GB-CREDITS      PIC S9(13)V99 COMP-3.
           05  GB-BALANCE      PIC S9(15)V99 COMP-3.
           05  FILLER          PIC X(3).
