      *> ====================================================================
      *> BALREC  - Account Balance (mutable)          DSORG=Indexed KSDS
      *> Keyed on AC-NO; kept separate from ACCTREC as a mainframe shop
      *> would, so balance updates never rewrite master attributes.
      *> ====================================================================
       01  BAL-REC.
           05  BA-NO            PIC X(10).
           05  BA-AVAIL         PIC S9(13)V99 COMP-3.
           05  BA-CURRENT       PIC S9(13)V99 COMP-3.
           05  BA-HOLD          PIC S9(13)V99 COMP-3.
           05  BA-OVERDRAFT     PIC S9(13)V99 COMP-3.
           05  BA-INT-ACCRUED   PIC S9(13)V99 COMP-3.
           05  BA-LAST-POST-DT  PIC 9(8).
           05  BA-LAST-TXN      PIC 9(12).
           05  BA-NSF-CNT       PIC 9(4) COMP-3.
           05  BA-FEE-CNT       PIC 9(4) COMP-3.
           05  BA-CLOSE-DT      PIC 9(8).
           05  FILLER           PIC X(8).
