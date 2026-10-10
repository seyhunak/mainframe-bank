      *> ====================================================================
      *> AUDREC  - Audit Trail                      DSORG=Sequential (Append)
      *> Written by every update path. Never rewritten. Old/new balances
      *> captured so the file is self-proving during reconciliation.
      *> ====================================================================
       01  AUD-REC.
           05  AU-SEQ           PIC 9(12).
           05  AU-DATE          PIC 9(8).
           05  AU-TIME          PIC 9(6).
           05  AU-PROG          PIC X(8).
           05  AU-USER          PIC X(8).
           05  AU-TERMINAL      PIC X(4).
           05  AU-ACTION        PIC X(1).
           05  AU-KEY           PIC X(12).
           05  AU-AMOUNT        PIC S9(13)V99 COMP-3.
           05  AU-OLD-BAL       PIC S9(13)V99 COMP-3.
           05  AU-NEW-BAL       PIC S9(13)V99 COMP-3.
           05  AU-MSG           PIC X(60).
           05  FILLER           PIC X(12).
