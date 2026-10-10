      *> ====================================================================
      *> STMTREC - Account Statement lines        DSORG=Sequential (Append)
      *> Produced by the nightly statement job, one line per posting.
      *> ====================================================================
       01  STMT-REC.
           05  SM-ACCT          PIC X(10).
           05  SM-CUST-ID       PIC X(10).
           05  SM-DATE          PIC 9(8).
           05  SM-SEQ           PIC 9(12).
           05  SM-TYPE          PIC X(2).
           05  SM-DEBIT         PIC S9(13)V99 COMP-3.
           05  SM-CREDIT        PIC S9(13)V99 COMP-3.
           05  SM-BALANCE       PIC S9(13)V99 COMP-3.
           05  SM-DESCR         PIC X(40).
