      *> ====================================================================
      *> POSTCOMM - COMMAREA for MBPOSTNG, the single posting engine.
      *> Every financial movement in the bank funnels through this module so
      *> that balance, journal and audit writes are centralised.
      *> ====================================================================
       01  POSTCOMM.
           05  POST-REQ.
               10  PR-TYPE       PIC X(2).
               10  PR-ACCT       PIC X(10).
               10  PR-ACCT2      PIC X(10).
               10  PR-LOANNO     PIC X(12).
               10  PR-CUST-ID    PIC X(10).
               10  PR-AMOUNT     PIC S9(13)V99 COMP-3.
               10  PR-FEE        PIC S9(13)V99 COMP-3.
               10  PR-CHANNEL    PIC X(2).
               10  PR-USER       PIC X(8).
               10  PR-TERM       PIC X(4).
               10  PR-TELLER     PIC X(6).
               10  PR-BRANCH     PIC 9(4).
               10  PR-DESCR      PIC X(40).
               10  PR-DATE       PIC 9(8).
               10  PR-TIME       PIC 9(6).
               10  PR-AUTH       PIC X(1).
               10  PR-SUPV       PIC X(8).
               10  PR-PROG       PIC X(8).
               10  PR-LOCK       PIC 9.
               10  PR-LEVEL      PIC 9.
               10  PR-LIMIT      PIC S9(13)V99 COMP-3.
               10  PR-CASH-LIMIT PIC S9(13)V99 COMP-3.
           05  POST-RESP.
               10  PY-RC         PIC S9(4) COMP.
               10  PY-MSG        PIC X(72).
               10  PY-SEQ        PIC 9(12).
               10  PY-OLD-BAL    PIC S9(13)V99 COMP-3.
               10  PY-NEW-BAL    PIC S9(13)V99 COMP-3.
               10  PY-FEE-CHG    PIC S9(13)V99 COMP-3.
               10  PY-POSTED     PIC 9(5) COMP-3.