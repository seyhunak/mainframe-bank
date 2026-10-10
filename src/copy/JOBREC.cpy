      *> ====================================================================
      *> JOBREC - Batch job run log                 DSORG=Sequential (Append)
      *> One line per night-end run: what ran, on which day, with what
      *> counts. The ledger balance line is what an auditor asks for.
      *> ====================================================================
       01  JOB-REC.
           05  JO-DATE         PIC 9(8).
           05  JO-JOB          PIC X(10).
           05  JO-RUNDATE      PIC 9(8).
           05  JO-LOANS        PIC 9(6).
           05  JO-INTACCR      PIC 9(6).
           05  JO-STATMNT      PIC 9(6).
           05  JO-GLACCTS      PIC 9(4).
           05  JO-GLBAL        PIC S9(15)V99 COMP-3.
           05  JO-LOCKS        PIC 9(6).
           05  FILLER          PIC X(20).
