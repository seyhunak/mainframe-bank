>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBJB001 - Night-end batch job.
       *>
       *> Runs after the online day is over, in the order a real core runs
       *> it:
       *>   1. interest accrual   - a day's interest on every loan, posted
       *>                          into the accrued-interest field
       *>   2. statement lines    - one line per journal entry, so the
       *>                          monthly statement program has its input
       *>   3. ledger proof       - the signed balance of every control
       *>                          account has to be zero
       *>   4. stale lock sweep    - a crashed terminal left a lock behind
       *>   5. job log            - what ran, when, and with what counts
       *>
       *> Restart: the run date is held in the counter dataset, so a re-run
       *> on the same day is refused unless the operator passes OVERRIDE.
       *>
       *> Run:  bin/NIGHTEND
       *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBJB001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT LOAN-FILE ASSIGN TO "data/LOANFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS LN-NO
               FILE STATUS IS FS-LOAN.
           SELECT LNBAL-FILE ASSIGN TO "data/LNBALFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LB-NO
               FILE STATUS IS FS-LNBAL.
           SELECT TXN-FILE ASSIGN TO "data/TXNFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TX-SEQ
               FILE STATUS IS FS-TXN.
           SELECT STMT-FILE ASSIGN TO "data/STMTFILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-STMT.
           SELECT SEQ-FILE ASSIGN TO "data/SEQFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SQ-NAME
               FILE STATUS IS FS-SEQ.
           SELECT GLB-FILE ASSIGN TO "data/GLBFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GB-ACCT
               FILE STATUS IS FS-GLB.
           SELECT LOCK-FILE ASSIGN TO "data/LOCKFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LK-KEY
               FILE STATUS IS FS-LOCK.
           SELECT JOBLOG-FILE ASSIGN TO "data/JOBLOG"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JOB.

       DATA DIVISION.
       FILE SECTION.
       FD  LOAN-FILE.
       COPY LOANREC.
       FD  LNBAL-FILE.
       COPY LNBALREC.
       FD  TXN-FILE.
       COPY TXNREC.
       FD  STMT-FILE.
       COPY STMTREC.
       FD  SEQ-FILE.
       COPY SEQREC.
       FD  GLB-FILE.
       COPY GLBREC.
       FD  LOCK-FILE.
       COPY LOCKREC.
       FD  JOBLOG-FILE.
       COPY JOBREC.

       WORKING-STORAGE SECTION.
       COPY BANKCON.
       COPY INTCOMM.
       COPY PARMCOMM.

       01  FS-LOAN     PIC XX.
       01  FS-LNBAL    PIC XX.
       01  FS-TXN      PIC XX.
       01  FS-STMT     PIC XX.
       01  FS-SEQ      PIC XX.
       01  FS-GLB      PIC XX.
       01  FS-LOCK     PIC XX.
       01  FS-JOB      PIC XX.

       01  W-RUN-DT    PIC 9(8).
       01  W-LAST-RUN  PIC 9(8).
       01  W-OVERRIDE  PIC X VALUE "N".
       01  W-INT-DAY   PIC S9(13)V99 COMP-3.
       01  W-LOAN-N    PIC 9(6) VALUE 0.
       01  W-INT-N     PIC 9(6) VALUE 0.
       01  W-STMT-N    PIC 9(6) VALUE 0.
       01  W-LOCK-N    PIC 9(6) VALUE 0.
       01  W-GL-ACCTS   PIC 9(4) VALUE 0.
       01  W-GL-DR     PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-GL-CR     PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-GL-BAL    PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-GL-ZERO   PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-TXN-EOF   PIC X VALUE "N".
       01  W-LOCK-EOF  PIC X VALUE "N".
       01  W-GLB-EOF   PIC X VALUE "N".
       01  W-TMP       PIC S9(9) COMP-3.

       PROCEDURE DIVISION.
       MAIN-PARA.
           ACCEPT W-RUN-DT FROM DATE YYYYMMDD
           ACCEPT W-OVERRIDE FROM COMMAND-LINE
           DISPLAY " "
           DISPLAY "==========================================================="
           DISPLAY "  MBJB001   NIGHT-END BATCH   " W-RUN-DT
           DISPLAY "==========================================================="
           PERFORM READ-LAST-RUN
           IF W-OVERRIDE NOT = "Y" AND W-LAST-RUN = W-RUN-DT
               DISPLAY "   ALREADY RUN TODAY - PASS OVERRIDE TO RE-RUN"
               PERFORM WRITE-JOBLOG
               GOBACK
           END-IF
           OPEN I-O LOAN-FILE LNBAL-FILE SEQ-FILE GLB-FILE LOCK-FILE
           OPEN INPUT TXN-FILE
           OPEN EXTEND STMT-FILE JOBLOG-FILE
           DISPLAY "   1. interest accrual"
           PERFORM ACCRUE-INTEREST
           DISPLAY "   2. statement lines"
           PERFORM BUILD-STATEMENTS
           DISPLAY "   3. ledger proof"
           PERFORM PROVE-LEDGER
           DISPLAY "   4. stale lock sweep"
           PERFORM SWEEP-LOCKS
           PERFORM BUMP-RUN-DATE
           PERFORM WRITE-JOBLOG
           CLOSE LOAN-FILE LNBAL-FILE SEQ-FILE GLB-FILE LOCK-FILE
           CLOSE TXN-FILE STMT-FILE JOBLOG-FILE
           GOBACK.

       READ-LAST-RUN.
           MOVE "JBRUN" TO SQ-NAME
           READ SEQ-FILE
           IF FS-SEQ = "23"
               MOVE 0 TO SQ-VALUE
               MOVE 0 TO W-LAST-RUN
               WRITE SEQ-REC
           ELSE
               MOVE SQ-VALUE TO W-TMP
               MOVE W-TMP TO W-LAST-RUN
           END-IF.

       BUMP-RUN-DATE.
           MOVE "JBRUN" TO SQ-NAME
           READ SEQ-FILE
           MOVE W-RUN-DT TO SQ-VALUE
           REWRITE SEQ-REC.

           *> ---- 1. one day's interest on every loan ------------------------
           *> The engine applies payments to accrued interest first, so the
           *> nightly job is what puts the day's interest into the field.
       ACCRUE-INTEREST.
           MOVE "INT-BASIS" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           IF PM-RC = 0
               MOVE PM-VALUE TO IC-AMT2
           ELSE
               MOVE 365 TO IC-AMT2
           END-IF
           MOVE SPACES TO LN-NO
           START LOAN-FILE KEY IS NOT LESS THAN LN-NO
               INVALID KEY
                   DISPLAY "   WARNING loan start failed, FS=" FS-LOAN
           END-START
           MOVE "N" TO W-TXN-EOF
           PERFORM UNTIL W-TXN-EOF = "Y"
               READ LOAN-FILE NEXT RECORD
                   AT END MOVE "Y" TO W-TXN-EOF
                   NOT AT END
                       IF LN-STATUS = "A"
                           ADD 1 TO W-LOAN-N
                           MOVE LN-NO TO LB-NO
                           READ LNBAL-FILE
                           IF FS-LNBAL = "00"
                               PERFORM POST-DAY-INTEREST
                           END-IF
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY "      loans scanned  =" W-LOAN-N
           DISPLAY "      interest posted=" W-INT-N.

       POST-DAY-INTEREST.
           MOVE LB-CUR-PRIN TO IC-AMT1
           MOVE LN-RATE TO IC-RATE
           MOVE 1 TO IC-N
           MOVE ICF-SIMPLE TO IC-FUNC
           CALL "MBINTCAL" USING INTCOMM
           IF IC-RC NOT = 0
               DISPLAY "      WARNING interest failed on " LN-NO
               EXIT PARAGRAPH
           END-IF
           MOVE IC-OUT TO W-INT-DAY
           IF W-INT-DAY > LB-CUR-INT
               MOVE 0 TO W-INT-DAY
           END-IF
           ADD W-INT-DAY TO LB-CUR-INT
           ADD W-INT-DAY TO LB-ACCR-INT-YTD
           MOVE W-RUN-DT TO LB-LAST-ACC-DT
           REWRITE LNBAL-REC
           IF FS-LNBAL NOT = "00"
               DISPLAY "      WARNING loan balance write FS=" FS-LNBAL
           END-IF
           ADD 1 TO W-INT-N.

           *> ---- 2. one statement line per journal entry ---------------------
       BUILD-STATEMENTS.
           MOVE "N" TO W-TXN-EOF
           PERFORM UNTIL W-TXN-EOF = "Y"
               READ TXN-FILE NEXT RECORD
                   AT END MOVE "Y" TO W-TXN-EOF
                   NOT AT END
                       IF TX-STATUS = "P"
                           PERFORM WRITE-STATEMENT-LINE
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY "      journal entries=" W-STMT-N.

       WRITE-STATEMENT-LINE.
           MOVE SPACES TO STMT-REC
           MOVE TX-ACCT TO SM-ACCT
           MOVE TX-CUST-ID TO SM-CUST-ID
           MOVE TX-DATE TO SM-DATE
           MOVE TX-SEQ TO SM-SEQ
           MOVE TX-TYPE TO SM-TYPE
           MOVE 0 TO SM-DEBIT
           MOVE 0 TO SM-CREDIT
           MOVE TX-DESCR TO SM-DESCR
           IF TX-TYPE = TT-WDL OR TX-TYPE = TT-LOANPM
               MOVE TX-AMOUNT TO SM-DEBIT
           ELSE
               MOVE TX-AMOUNT TO SM-CREDIT
           END-IF
           MOVE TX-BAL-AFTER TO SM-BALANCE
           WRITE STMT-REC
           IF FS-STMT NOT = "00"
               DISPLAY "      WARNING statement write FS=" FS-STMT
           END-IF
           ADD 1 TO W-STMT-N.

           *> ---- 3. the ledger has to balance -------------------------------
           *> Debits equal credits by construction, so the signed balance of
           *> every control account is zero and the total across the chart
           *> is zero. Anything else is an out-of-balance condition and the
           *> job reports it rather than papering over it.
       PROVE-LEDGER.
           MOVE SPACES TO GB-ACCT
           START GLB-FILE KEY IS NOT LESS THAN GB-ACCT
               INVALID KEY
                   DISPLAY "   WARNING ledger start failed, FS=" FS-GLB
           END-START
           MOVE "N" TO W-GLB-EOF
           PERFORM UNTIL W-GLB-EOF = "Y"
               READ GLB-FILE NEXT RECORD
                   AT END MOVE "Y" TO W-GLB-EOF
                   NOT AT END
                       ADD 1 TO W-GL-ACCTS
                       ADD GB-DEBITS TO W-GL-DR
                       ADD GB-CREDITS TO W-GL-CR
                       ADD GB-BALANCE TO W-GL-BAL
                       DISPLAY "      " GB-ACCT " " GB-NAME
                               "  dr=" GB-DEBITS " cr=" GB-CREDITS
                               " bal=" GB-BALANCE
               END-READ
           END-PERFORM
           DISPLAY "      control accounts =" W-GL-ACCTS
           DISPLAY "      debits  =" W-GL-DR
           DISPLAY "      credits =" W-GL-CR
           DISPLAY "      net     =" W-GL-BAL
           IF W-GL-BAL NOT = 0
               DISPLAY "   *** LEDGER OUT OF BALANCE - DO NOT CLOSE THE DAY ***"
           ELSE
               DISPLAY "      ledger in balance"
           END-IF.

           *> ---- 4. locks a crashed task left behind ------------------------
       SWEEP-LOCKS.
           MOVE "N" TO W-LOCK-EOF
           PERFORM UNTIL W-LOCK-EOF = "Y"
               READ LOCK-FILE NEXT RECORD
                   AT END MOVE "Y" TO W-LOCK-EOF
                   NOT AT END
                       DELETE LOCK-FILE RECORD
                       IF FS-LOCK = "00"
                           ADD 1 TO W-LOCK-N
                       END-IF
               END-READ
           END-PERFORM
           DISPLAY "      locks swept =" W-LOCK-N.

           *> ---- 5. what ran, when, with what counts -------------------------
       WRITE-JOBLOG.
           MOVE SPACES TO JOB-REC
           MOVE W-RUN-DT TO JO-DATE
           MOVE "NIGHT-END" TO JO-JOB
           MOVE W-RUN-DT TO JO-RUNDATE
           MOVE W-LOAN-N TO JO-LOANS
           MOVE W-INT-N TO JO-INTACCR
           MOVE W-STMT-N TO JO-STATMNT
           MOVE W-GL-ACCTS TO JO-GLACCTS
           MOVE W-GL-BAL TO JO-GLBAL
           MOVE W-LOCK-N TO JO-LOCKS
           WRITE JOB-REC
           IF FS-JOB NOT = "00"
               DISPLAY "   WARNING job log write FS=" FS-JOB
           END-IF.
