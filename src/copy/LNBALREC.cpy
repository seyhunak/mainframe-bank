      *> ====================================================================
      *> LNBALREC - Loan Balance & Delinquency      DSORG=Indexed KSDS
      *> ====================================================================
       01  LNBAL-REC.
           05  LB-NO            PIC X(12).
           05  LB-CUR-PRIN      PIC S9(13)V99 COMP-3.
           05  LB-CUR-INT       PIC S9(13)V99 COMP-3.
           05  LB-ACCR-INT-YTD  PIC S9(13)V99 COMP-3.
           05  LB-PAID-PRIN     PIC S9(13)V99 COMP-3.
           05  LB-PAID-INT      PIC S9(13)V99 COMP-3.
           05  LB-PMTS-MADE     PIC 9(4) COMP-3.
           05  LB-LAST-PMNT-DT  PIC 9(8).
           05  LB-NEXT-DUE-DT   PIC 9(8).
           05  LB-DELINQ        PIC X(1).
           05  LB-DAYS-PAST-DUE PIC 9(4) COMP-3.
           05  LB-LAST-ACC-DT   PIC 9(8).
           05  FILLER           PIC X(8).
