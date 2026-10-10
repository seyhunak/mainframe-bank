      *> ====================================================================
      *> TXNREC  - Transaction Journal              DSORG=Indexed KSDS
      *> One immutable record per financial posting, keyed by sequence.
      *> Append-only in practice; updated only to mark reversal status.
      *> ====================================================================
       01  TXN-REC.
           05  TX-SEQ           PIC 9(12).
           05  TX-DATE          PIC 9(8).
           05  TX-TIME          PIC 9(6).
           05  TX-TYPE          PIC X(2).
           05  TX-ACCT          PIC X(10).
           05  TX-CUST-ID       PIC X(10).
           05  TX-AMOUNT        PIC S9(13)V99 COMP-3.
           05  TX-BAL-BEFORE    PIC S9(13)V99 COMP-3.
           05  TX-BAL-AFTER     PIC S9(13)V99 COMP-3.
           05  TX-REF           PIC X(12).
           05  TX-CHANNEL       PIC X(2).
           05  TX-TERMINAL      PIC X(4).
           05  TX-TELLER        PIC X(6).
           05  TX-BRANCH        PIC 9(4).
           05  TX-DESCR         PIC X(40).
           05  TX-STATUS        PIC X(1).
           05  TX-PROG          PIC X(8).
           05  FILLER           PIC X(8).
       01  TX-STATUS-DEF.
           05  TXS-POST         PIC X VALUE "P".
           05  TXS-REVERSED     PIC X VALUE "R".
           05  TXS-PENDING      PIC X VALUE "H".
