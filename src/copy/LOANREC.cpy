      *> ====================================================================
      *> LOANREC - Loan Master                     DSORG=Indexed KSDS
      *> ====================================================================
       01  LOAN-REC.
           05  LN-NO            PIC X(12).
           05  LN-CUST-ID       PIC X(10).
           05  LN-DISB-ACCT     PIC X(10).
           05  LN-TYPE          PIC X(1).
           05  LN-STATUS        PIC X(1).
           05  LN-OPEN-DT       PIC 9(8).
           05  LN-MATURITY-DT   PIC 9(8).
           05  LN-LAST-PMNT-DT  PIC 9(8).
           05  LN-ORIGINAL      PIC S9(13)V99 COMP-3.
           05  LN-RATE          PIC S9(5)V9(10) COMP-3.
           05  LN-TERM-MO       PIC 9(3) COMP-3.
           05  LN-PAYMENT       PIC S9(13)V99 COMP-3.
           05  LN-DUE-DAY       PIC 9(2) COMP-3.
           05  LN-BRANCH        PIC 9(4).
           05  LN-COLLATERAL    PIC S9(13)V99 COMP-3.
           05  LN-NMEMO         PIC X(40).
           05  FILLER           PIC X(8).
