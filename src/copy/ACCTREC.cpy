      *> ====================================================================
      *> ACCTREC - Account Master (static attributes)  DSORG=Indexed KSDS
      *> ====================================================================
       01  ACCT-REC.
           05  AC-NO            PIC X(10).
           05  AC-CUST-ID       PIC X(10).
           05  AC-TYPE          PIC X(1).
           05  AC-STATUS        PIC X(1).
           05  AC-OPEN-DT       PIC 9(8).
           05  AC-CLOSE-DT      PIC 9(8).
           05  AC-BRANCH        PIC 9(4).
           05  AC-TITLE         PIC X(20).
           05  AC-MIN-BAL       PIC S9(13)V99 COMP-3.
           05  AC-OD-LIMIT      PIC S9(13)V99 COMP-3.
           05  AC-INT-RATE      PIC S9(5)V9(10) COMP-3.
           05  AC-LTD-CRT       PIC S9(13)V99 COMP-3.
           05  AC-LTD-DBT       PIC S9(13)V99 COMP-3.
           05  AC-PREV-BAL      PIC S9(13)V99 COMP-3.
           05  AC-MEMO          PIC X(40).
           05  FILLER           PIC X(8).
