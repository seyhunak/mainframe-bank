      *> ====================================================================
      *> TELLREC - Teller / operator master           DSORG=Indexed KSDS
      *> Holds who may sign on, at what branch, up to what amount, and the
      *> authority level that decides whether an overdraft or a large cash
      *> movement needs a supervisor's authorisation.
      *> ====================================================================
       01  TELLER-REC.
           05  TE-ID            PIC X(8).
           05  TE-NAME          PIC X(30).
           05  TE-PASSWD        PIC X(8).
           05  TE-LEVEL         PIC 9.
           05  TE-STATUS        PIC X(1).
           05  TE-BRANCH        PIC 9(4).
           05  TE-LIMIT         PIC S9(13)V99 COMP-3.
           05  TE-CASH-LIMIT    PIC S9(13)V99 COMP-3.
           05  TE-LAST-DT       PIC 9(8).
           05  TE-LOCK-CNT      PIC 9(2) COMP VALUE 0.
           05  FILLER           PIC X(12).
