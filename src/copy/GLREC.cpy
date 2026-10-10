      *> ====================================================================
      *> GLREC  - General ledger entries             DSORG=Sequential (Append)
      *> One record per debit or credit, in pairs. Every posting the engine
      *> makes writes exactly one DR line and one CR line for the same
      *> amount, so the sum of all amounts signed by side is always zero.
      *> ====================================================================
       01  GL-REC.
           05  GL-SEQ          PIC 9(12) COMP-3.
           05  GL-DATE         PIC 9(8).
           05  GL-TIME         PIC 9(6).
           05  GL-TXN-SEQ      PIC 9(12) COMP-3.
           05  GL-PROG         PIC X(8).
           05  GL-USER         PIC X(8).
           05  GL-TERM         PIC X(4).
           05  GL-ACCT         PIC X(8).
           05  GL-SIDE         PIC X(1).
           05  GL-SUBACCT      PIC X(10).
           05  GL-AMOUNT       PIC S9(13)V99 COMP-3.
           05  GL-DESCR        PIC X(40).
           05  FILLER          PIC X(7).
