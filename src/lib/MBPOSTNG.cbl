>>SOURCE FORMAT FREE
      *> ====================================================================
      *> MBPOSTNG - THE POSTING ENGINE.
      *>
      *> This is the only program in the bank that writes a balance. Every
      *> teller transaction, every automated posting and every batch run
      *> reaches the ledger by building a POSTCOMM request and calling here.
      *> Concentrating the write path is what makes the audit trail complete:
      *> there is exactly one place where a balance can change, so there is
      *> exactly one place where the old and new figures get journalled.
      *>
      *> On a successful posting the engine touches, in this order:
      *>   1. the balance dataset      (BA-AVAIL / BA-CURRENT)
      *>   2. the account master lifetime credit/debit counters
      *>   3. the transaction journal  (immutable, keyed by sequence)
      *>   4. the audit trail          (append only, old and new balances)
      *>
      *> The engine owns its file set so the datasets cannot drift apart
      *> between programs.
      *>
      *> COMMAREA dispatch: fill POSTCOMM, CALL "MBPOSTNG", read POSTCOMM.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. MBPOSTNG.

ENVIRONMENT DIVISION.
INPUT-OUTPUT SECTION.
FILE-CONTROL.
    SELECT ACCT-FILE ASSIGN TO "data/ACCTFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS AC-NO
        FILE STATUS IS FS-ACCT.
    SELECT BAL-FILE ASSIGN TO "data/BALFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS BA-NO
        FILE STATUS IS FS-BAL.
    SELECT LOAN-FILE ASSIGN TO "data/LOANFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS LN-NO
        FILE STATUS IS FS-LOAN.
    SELECT LNBAL-FILE ASSIGN TO "data/LNBALFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS LB-NO
        FILE STATUS IS FS-LNBAL.
    SELECT TXN-FILE ASSIGN TO "data/TXNFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS TX-SEQ
        FILE STATUS IS FS-TXN.
    SELECT AUD-FILE ASSIGN TO "data/AUDFILE"
        ORGANIZATION IS SEQUENTIAL
        FILE STATUS IS FS-AUD.
    SELECT SEQ-FILE ASSIGN TO "data/SEQFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS SQ-NAME
        FILE STATUS IS FS-SEQ.
    SELECT GL-FILE ASSIGN TO "data/GLFILE"
        ORGANIZATION IS SEQUENTIAL
        FILE STATUS IS FS-GL.
    SELECT LOCK-FILE ASSIGN TO "data/LOCKFILE"
        ORGANIZATION IS INDEXED
        ACCESS MODE IS RANDOM
        RECORD KEY IS LK-KEY
        FILE STATUS IS FS-LOCK.

DATA DIVISION.
FILE SECTION.
FD  ACCT-FILE.
COPY ACCTREC.
FD  BAL-FILE.
COPY BALREC.
FD  LOAN-FILE.
COPY LOANREC.
FD  LNBAL-FILE.
COPY LNBALREC.
FD  TXN-FILE.
COPY TXNREC.
FD  AUD-FILE.
COPY AUDREC.
FD  SEQ-FILE.
COPY SEQREC.
FD  GL-FILE.
COPY GLREC.
FD  LOCK-FILE.
COPY LOCKREC.

WORKING-STORAGE SECTION.
      *> shared code tables: transaction types, statuses, return codes
COPY BANKCON.
COPY PARMCOMM.
COPY GLCOMM.

01  FS-ACCT     PIC XX.
01  FS-BAL      PIC XX.
01  FS-LOAN     PIC XX.
01  FS-LNBAL    PIC XX.
01  FS-TXN      PIC XX.
01  FS-AUD      PIC XX.
01  FS-SEQ      PIC XX.
01  FS-GL       PIC XX.
01  FS-LOCK     PIC XX.

01  W-LEG-KEY    PIC X(10).
01  W-LEG-KEY2   PIC X(10).
01  W-LEG-CUST   PIC X(10).

01  W-OLD        PIC S9(13)V99 COMP-3.
01  W-NEW        PIC S9(13)V99 COMP-3.
01  W-OLD2       PIC S9(13)V99 COMP-3.
01  W-NEW2       PIC S9(13)V99 COMP-3.
01  W-AMT        PIC S9(13)V99 COMP-3.
01  W-PRIN       PIC S9(13)V99 COMP-3.
01  W-INTAMT     PIC S9(13)V99 COMP-3.
01  W-PMTOT      PIC S9(13)V99 COMP-3.
01  W-DR         PIC X.
01  W-TXNSEQ     PIC 9(12).
01  W-AUDSEQ     PIC 9(12).
01  W-GLSEQ      PIC 9(12).
01  W-DT         PIC 9(8).
01  W-TM         PIC 9(6).
01  W-INTED      PIC -Z(11)9.99.
01  W-PRINED     PIC -Z(11)9.99.
01  W-NSF-FEE    PIC S9(13)V99 COMP-3 VALUE 25.00.
01  W-SAVACCT    PIC X(10).
      *> ledger side of the posting in progress: which control account the
      *> debit lands in and which control account the credit lands in
01  W-GL-DR      PIC X(8).
01  W-GL-CR      PIC X(8).
01  W-GL-DRNM    PIC X(30).
01  W-GL-CRNM    PIC X(30).
01  W-LOCK-HELD  PIC X VALUE "N".
01  W-LOCK-AGE   PIC S9(9) COMP-3 VALUE 5.
01  LOCK-IS-STALE PIC X VALUE "N".
01  W-LOCK-KEY   PIC X(12).
01  W-LOCK2-HELD PIC X VALUE "N".
01  W-SECS       PIC S9(9) COMP VALUE 0.
01  W-HH         PIC S9(9) COMP VALUE 0.
01  W-MM         PIC S9(9) COMP VALUE 0.
01  W-SS         PIC S9(9) COMP VALUE 0.
01  W-REST       PIC S9(9) COMP VALUE 0.
01  W-LCK-SECS   PIC S9(9) COMP VALUE 0.
01  W-CUR-SECS   PIC S9(9) COMP VALUE 0.
01  W-DIFF       PIC S9(9) COMP VALUE 0.
01  W-LCK-SECS-IN PIC 9(6).
01  W-CUR-SECS-IN PIC 9(6).

LINKAGE SECTION.
COPY POSTCOMM.

PROCEDURE DIVISION USING POSTCOMM.

MAIN-PARA.
    MOVE 0 TO PY-RC
    MOVE SPACES TO PY-MSG
    MOVE 0 TO PY-POSTED
    MOVE 0 TO PY-FEE-CHG
    IF PR-DATE = 0
        ACCEPT PR-DATE FROM DATE YYYYMMDD
    END-IF
    IF PR-TIME = 0
        ACCEPT PR-TIME FROM TIME
    END-IF
    MOVE PR-DATE TO W-DT
    MOVE PR-TIME TO W-TM
      *> the NSF fee is a parameter now, not a literal in the program
    MOVE "NSF-FEE" TO PM-NAME
    CALL "MBPARM" USING PARMCOMM
    IF PM-RC = 0
        MOVE PM-VALUE TO W-NSF-FEE
    END-IF
    PERFORM OPEN-FILES
    IF PY-RC NOT = 0
        PERFORM CLOSE-FILES
        GOBACK
    END-IF
      *> take the accounts out of circulation before anything is read
    IF PR-LOCK = 1
        MOVE PR-ACCT TO W-LOCK-KEY
        PERFORM ACQUIRE-LOCK
        IF PY-RC NOT = 0
            PERFORM CLOSE-FILES
            GOBACK
        END-IF
        IF PR-TYPE = TT-XFR AND PR-ACCT2 NOT = PR-ACCT
            MOVE PR-ACCT2 TO W-LOCK-KEY
            PERFORM ACQUIRE-LOCK
            IF PY-RC NOT = 0
                PERFORM RELEASE-LOCK
                MOVE PR-ACCT TO W-LOCK-KEY
                PERFORM RELEASE-LOCK
                PERFORM CLOSE-FILES
                GOBACK
            END-IF
            MOVE "Y" TO W-LOCK2-HELD
        END-IF
    END-IF
    EVALUATE PR-TYPE
        WHEN TT-DEP      PERFORM POST-DEPOSIT
        WHEN TT-WDL      PERFORM POST-WITHDRAWAL
        WHEN TT-XFR      PERFORM POST-TRANSFER
        WHEN TT-INTCR    PERFORM POST-CREDIT
        WHEN TT-FEE      PERFORM POST-FEE
        WHEN TT-NSF      PERFORM POST-NSF
        WHEN TT-LOANDD   PERFORM POST-DISBURSE
        WHEN TT-LOANPM    PERFORM POST-LOANPAY
        WHEN OTHER
            MOVE 48 TO PY-RC
            MOVE "POSTING: UNKNOWN TRANSACTION TYPE" TO PY-MSG
    END-EVALUATE
      *> the operator's authority is the last gate: a movement that is fine
      *> on funds still has to be inside the teller's own limit
    IF PY-RC = 0 AND PR-AUTH NOT = "Y"
        PERFORM CHECK-AUTHORITY
    END-IF
    IF PR-LOCK = 1
        IF W-LOCK2-HELD = "Y"
            MOVE PR-ACCT2 TO W-LOCK-KEY
            PERFORM RELEASE-LOCK
            MOVE "N" TO W-LOCK2-HELD
        END-IF
        MOVE PR-ACCT TO W-LOCK-KEY
        PERFORM RELEASE-LOCK
    END-IF
    PERFORM CLOSE-FILES
    GOBACK.

      *> every account movement is bounded by the teller's limit; a cash
      *> movement is bounded again by the cash limit, which is stricter
CHECK-AUTHORITY.
    EVALUATE PR-TYPE
        WHEN TT-DEP WHEN TT-WDL WHEN TT-XFR WHEN TT-LOANPM
            IF PR-LIMIT > 0 AND PR-AMOUNT > PR-LIMIT
                MOVE 60 TO PY-RC
                MOVE "AMOUNT EXCEEDS OPERATOR AUTHORITY" TO PY-MSG
            END-IF
    END-EVALUATE
    IF PR-TYPE = TT-DEP OR PR-TYPE = TT-WDL
        IF PR-CASH-LIMIT > 0 AND PR-AMOUNT > PR-CASH-LIMIT
            MOVE 60 TO PY-RC
            MOVE "AMOUNT EXCEEDS CASH AUTHORITY" TO PY-MSG
        END-IF
    END-IF.

OPEN-FILES.
    OPEN I-O ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE TXN-FILE SEQ-FILE
    IF FS-ACCT NOT = "00" OR FS-BAL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "POSTING: CANNOT OPEN ACCOUNT DATASETS" TO PY-MSG
    END-IF
    IF FS-TXN NOT = "00" OR FS-SEQ NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "POSTING: CANNOT OPEN JOURNAL DATASETS" TO PY-MSG
    END-IF
    OPEN EXTEND AUD-FILE
    IF FS-AUD NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "POSTING: CANNOT OPEN AUDIT TRAIL" TO PY-MSG
    END-IF
    OPEN EXTEND GL-FILE
    IF FS-GL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "POSTING: CANNOT OPEN LEDGER FILE" TO PY-MSG
    END-IF
    IF PR-LOCK = 1
        OPEN I-O LOCK-FILE
        IF FS-LOCK NOT = "00"
            MOVE 40 TO PY-RC
            MOVE "POSTING: CANNOT OPEN LOCK DATASET" TO PY-MSG
        END-IF
    END-IF.

CLOSE-FILES.
    CLOSE ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE TXN-FILE SEQ-FILE
    CLOSE AUD-FILE GL-FILE.
    IF PR-LOCK = 1
        CLOSE LOCK-FILE
    END-IF.

      *> ==== record locking ===============================================
      *> A posting takes the account out of circulation for its own
      *> duration. A second task that finds the record there is told the
      *> account is in use rather than reading a balance that is about to
      *> change under it. Locks older than W-LOCK-AGE minutes are stale
      *> (a crashed task) and are taken over, which is what the night-end
      *> sweep reports on.
ACQUIRE-LOCK.
    MOVE W-LOCK-KEY TO LK-KEY
    READ LOCK-FILE
    IF FS-LOCK = "00"
        PERFORM CHECK-LOCK-AGE
        IF LOCK-IS-STALE = "N"
            MOVE 52 TO PY-RC
            MOVE "ACCOUNT IN USE BY ANOTHER TERMINAL" TO PY-MSG
            EXIT PARAGRAPH
        END-IF
        MOVE SPACES TO PY-MSG
    END-IF
    MOVE W-LOCK-KEY TO LK-KEY
    MOVE PR-TERM TO LK-TERM
    MOVE PR-USER TO LK-USER
    MOVE W-DT TO LK-DATE
    MOVE W-TM TO LK-TIME
    WRITE LOCK-REC
    IF FS-LOCK NOT = "00"
        MOVE 52 TO PY-RC
        MOVE "ACCOUNT IN USE BY ANOTHER TERMINAL" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE "Y" TO W-LOCK-HELD.

      *> a lock older than the parameter minutes belongs to a task that died
CHECK-LOCK-AGE.
    MOVE "N" TO LOCK-IS-STALE
    MOVE LK-TIME TO W-LCK-SECS-IN
    PERFORM HHMMSS-TO-SECS
    MOVE W-SECS TO W-LCK-SECS
    MOVE W-TM TO W-CUR-SECS-IN
    PERFORM HHMMSS-TO-SECS
    MOVE W-SECS TO W-CUR-SECS
    COMPUTE W-DIFF = W-CUR-SECS - W-LCK-SECS
    IF W-DIFF < 0
        ADD 86400 TO W-DIFF
    END-IF
    IF W-DIFF > W-LOCK-AGE * 60
        MOVE "Y" TO LOCK-IS-STALE
    END-IF.

      *> HHMMSS -> seconds since midnight, stepped with DIVIDE because a
      *> division inside a compound COMPUTE rounds where this means truncate
HHMMSS-TO-SECS.
    DIVIDE W-LCK-SECS-IN BY 10000 GIVING W-HH REMAINDER W-REST
    DIVIDE W-REST BY 100 GIVING W-MM REMAINDER W-SS
    COMPUTE W-SECS = (W-HH * 3600) + (W-MM * 60) + W-SS.

RELEASE-LOCK.
    IF W-LOCK-HELD = "Y"
        MOVE W-LOCK-KEY TO LK-KEY
        DELETE LOCK-FILE
        MOVE "N" TO W-LOCK-HELD
    END-IF.

      *> ==== sequence numbers =============================================
      *> The counter dataset is the shared source of journal numbers so no
      *> two postings can be handed the same sequence.
NEXT-TXNSEQ.
    MOVE "TXNSEQ" TO SQ-NAME
    READ SEQ-FILE
    EVALUATE FS-SEQ
        WHEN "23"
            MOVE 1 TO SQ-VALUE
            WRITE SEQ-REC
        WHEN "00"
            ADD 1 TO SQ-VALUE
            REWRITE SEQ-REC
        WHEN OTHER
            MOVE 40 TO PY-RC
            MOVE "COUNTER DATASET READ ERROR" TO PY-MSG
    END-EVALUATE
    MOVE SQ-VALUE TO W-TXNSEQ.

NEXT-AUDSEQ.
    MOVE "AUDSEQ" TO SQ-NAME
    READ SEQ-FILE
    EVALUATE FS-SEQ
        WHEN "23"
            MOVE 1 TO SQ-VALUE
            WRITE SEQ-REC
        WHEN "00"
            ADD 1 TO SQ-VALUE
            REWRITE SEQ-REC
        WHEN OTHER
            MOVE 40 TO PY-RC
            MOVE "COUNTER DATASET READ ERROR" TO PY-MSG
    END-EVALUATE
    MOVE SQ-VALUE TO W-AUDSEQ.

      *> the ledger draws its own sequence so entries can be traced back to
      *> the journal record that produced them
NEXT-GLSEQ.
    MOVE "GLSEQ" TO SQ-NAME
    READ SEQ-FILE
    EVALUATE FS-SEQ
        WHEN "23"
            MOVE 1 TO SQ-VALUE
            WRITE SEQ-REC
        WHEN "00"
            ADD 1 TO SQ-VALUE
            REWRITE SEQ-REC
        WHEN OTHER
            MOVE 40 TO PY-RC
            MOVE "COUNTER DATASET READ ERROR" TO PY-MSG
    END-EVALUATE
    MOVE SQ-VALUE TO W-GLSEQ.

      *> ==== reload an account pair by key ===============================
      *> One FD holds one record, so a two legged posting re-reads each
      *> side rather than holding two images at once. Every check happens
      *> before any write, so a re-read here cannot turn a rejected
      *> transfer into a half applied one.
LOAD-KEY.
    MOVE W-LEG-KEY TO AC-NO
    READ ACCT-FILE
    MOVE AC-NO TO BA-NO
    READ BAL-FILE
    MOVE AC-CUST-ID TO W-LEG-CUST.

      *> ==== read an account and its balance =============================
      *> ACCT-REC and BAL-REC are left loaded. PY-RC signals failure.
      *> W-DR is set to "C" or "D" so the lifetime counters are correct.
READ-ACCOUNT.
    MOVE 0 TO PY-RC
    MOVE SPACES TO PY-MSG
    MOVE PR-ACCT TO AC-NO
    READ ACCT-FILE
    IF FS-ACCT = "23"
        MOVE 4 TO PY-RC
        MOVE "ACCOUNT NOT FOUND" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF FS-ACCT NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR READING ACCOUNT MASTER" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF AC-STATUS NOT = "A"
        MOVE 16 TO PY-RC
        EVALUATE AC-STATUS
            WHEN "D" MOVE "ACCOUNT IS DORMANT" TO PY-MSG
            WHEN "C" MOVE "ACCOUNT IS CLOSED" TO PY-MSG
            WHEN "F" MOVE "ACCOUNT IS FROZEN" TO PY-MSG
            WHEN OTHER MOVE "ACCOUNT NOT ACTIVE" TO PY-MSG
        END-EVALUATE
        EXIT PARAGRAPH
    END-IF
    MOVE AC-NO TO BA-NO
    READ BAL-FILE
    IF FS-BAL = "23"
        MOVE 44 TO PY-RC
        MOVE "NO BALANCE RECORD - ACCOUNT NOT SETTLED" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF FS-BAL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR READING BALANCE" TO PY-MSG
    END-IF.

      *> ==== persist the balance, the master counters, journal and audit ==
      *> W-OLD/W-NEW carry the before and after figures, W-AMT the amount
      *> and W-DR the direction.
SAVE-AND-JOURNAL.
    MOVE W-NEW TO BA-AVAIL
    MOVE W-NEW TO BA-CURRENT
    MOVE W-DT TO BA-LAST-POST-DT
    MOVE W-TXNSEQ TO BA-LAST-TXN
    IF W-NEW < 0
        COMPUTE BA-OVERDRAFT = 0 - W-NEW
    ELSE
        MOVE 0 TO BA-OVERDRAFT
    END-IF
    REWRITE BAL-REC
    IF FS-BAL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR UPDATING BALANCE" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF W-DR = "C"
        ADD W-AMT TO AC-LTD-CRT
    ELSE
        ADD W-AMT TO AC-LTD-DBT
    END-IF
    MOVE W-NEW TO AC-PREV-BAL
    REWRITE ACCT-REC
    IF FS-ACCT NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR UPDATING ACCOUNT MASTER" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    PERFORM WRITE-JOURNAL
    PERFORM WRITE-LEDGER
    PERFORM WRITE-AUDIT
    MOVE W-OLD TO PY-OLD-BAL
    MOVE W-NEW TO PY-NEW-BAL.

      *> ==== double entry =================================================
      *> Exactly two lines per posting, same amount, one debit and one
      *> credit, so the signed sum of the ledger is always zero. The pair
      *> carries the journal sequence of the posting that produced it.
WRITE-LEDGER.
    IF W-GL-DR = SPACES OR W-GL-CR = SPACES
        MOVE 40 TO PY-RC
        MOVE "GL: POSTING HAS NO LEDGER SIDE" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    PERFORM NEXT-GLSEQ
    PERFORM WRITE-GL-LINE
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    PERFORM WRITE-GL-LINE-CR.

WRITE-GL-LINE.
    MOVE W-GLSEQ TO GL-SEQ
    MOVE W-DT TO GL-DATE
    MOVE W-TM TO GL-TIME
    MOVE W-TXNSEQ TO GL-TXN-SEQ
    MOVE PR-PROG TO GL-PROG
    MOVE PR-USER TO GL-USER
    MOVE PR-TERM TO GL-TERM
    MOVE W-GL-DR TO GL-ACCT OF GL-REC
    MOVE "D" TO GL-SIDE OF GL-REC
    MOVE PR-ACCT TO GL-SUBACCT
    MOVE W-AMT TO GL-AMOUNT OF GL-REC
    MOVE PR-DESCR TO GL-DESCR
    WRITE GL-REC
    IF FS-GL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR WRITING LEDGER" TO PY-MSG
    END-IF.

WRITE-GL-LINE-CR.
    MOVE W-GLSEQ TO GL-SEQ
    MOVE W-DT TO GL-DATE
    MOVE W-TM TO GL-TIME
    MOVE W-TXNSEQ TO GL-TXN-SEQ
    MOVE PR-PROG TO GL-PROG
    MOVE PR-USER TO GL-USER
    MOVE PR-TERM TO GL-TERM
    MOVE W-GL-CR TO GL-ACCT OF GL-REC
    MOVE "C" TO GL-SIDE OF GL-REC
    MOVE PR-ACCT TO GL-SUBACCT
    MOVE W-AMT TO GL-AMOUNT OF GL-REC
    MOVE PR-DESCR TO GL-DESCR
    WRITE GL-REC
    IF FS-GL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR WRITING LEDGER" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    PERFORM UPDATE-GL-BAL.

      *> keep the control account balances current as entries are written
UPDATE-GL-BAL.
    MOVE W-GL-DR TO GC-ACCT
    MOVE W-GL-DRNM TO GC-NAME
    MOVE "D" TO GC-SIDE
    MOVE W-AMT TO GC-AMOUNT
    CALL "MBGL" USING GLCOMM
    IF GC-RC NOT = 0
        MOVE 40 TO PY-RC
        MOVE "GL: CONTROL ACCOUNT UPDATE FAILED" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE W-GL-CR TO GC-ACCT
    MOVE W-GL-CRNM TO GC-NAME
    MOVE "C" TO GC-SIDE
    MOVE W-AMT TO GC-AMOUNT
    CALL "MBGL" USING GLCOMM
    IF GC-RC NOT = 0
        MOVE 40 TO PY-RC
        MOVE "GL: CONTROL ACCOUNT UPDATE FAILED" TO PY-MSG
    END-IF.

WRITE-JOURNAL.
    PERFORM NEXT-TXNSEQ
    MOVE W-TXNSEQ TO TX-SEQ
    MOVE W-DT TO TX-DATE
    MOVE W-TM TO TX-TIME
    MOVE PR-TYPE TO TX-TYPE
    MOVE PR-ACCT TO TX-ACCT
    MOVE AC-CUST-ID TO TX-CUST-ID
    MOVE W-AMT TO TX-AMOUNT
    MOVE W-OLD TO TX-BAL-BEFORE
    MOVE W-NEW TO TX-BAL-AFTER
    MOVE PR-LOANNO TO TX-REF
    MOVE PR-CHANNEL TO TX-CHANNEL
    MOVE PR-TERM TO TX-TERMINAL
    MOVE PR-TELLER TO TX-TELLER
    MOVE PR-BRANCH TO TX-BRANCH
    MOVE PR-DESCR TO TX-DESCR
    MOVE "P" TO TX-STATUS
    MOVE PR-PROG TO TX-PROG
    WRITE TXN-REC
    IF FS-TXN NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR WRITING JOURNAL" TO PY-MSG
    END-IF
    MOVE W-TXNSEQ TO PY-SEQ.

WRITE-AUDIT.
    PERFORM NEXT-AUDSEQ
    MOVE W-AUDSEQ TO AU-SEQ
    MOVE W-DT TO AU-DATE
    MOVE W-TM TO AU-TIME
    MOVE PR-PROG TO AU-PROG
    MOVE PR-USER TO AU-USER
    MOVE PR-TERM TO AU-TERMINAL
    MOVE "U" TO AU-ACTION
    MOVE PR-ACCT TO AU-KEY
    MOVE W-AMT TO AU-AMOUNT
    MOVE W-OLD TO AU-OLD-BAL
    MOVE W-NEW TO AU-NEW-BAL
    MOVE PY-MSG TO AU-MSG
    WRITE AUD-REC
    IF FS-AUD NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR WRITING AUDIT TRAIL" TO PY-MSG
    END-IF.

      *> ==== DEPOSIT =====================================================
POST-DEPOSIT.
    MOVE GA-CASH TO W-GL-DR
    MOVE "CASH ON HAND" TO W-GL-DRNM
    MOVE GA-DEP TO W-GL-CR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-CRNM
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    IF PR-AMOUNT <= 0
        MOVE 12 TO PY-RC
        MOVE "DEPOSIT AMOUNT MUST BE GREATER THAN ZERO" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD
    ADD PR-AMOUNT TO W-OLD
    MOVE W-OLD TO W-NEW
    MOVE PR-AMOUNT TO W-AMT
    MOVE "C" TO W-DR
    MOVE "CASH DEPOSIT" TO PR-DESCR
    MOVE "DEPOSIT ACCEPTED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC = 0
        MOVE 1 TO PY-POSTED
    END-IF.

      *> ==== WITHDRAWAL ==================================================
      *> Checks are in this order: account usable, amount positive, then
      *> funds. An overdraft limit permits a negative balance but never
      *> beyond the limit; otherwise the item is returned and the account
      *> pays the charge.
POST-WITHDRAWAL.
    MOVE GA-DEP TO W-GL-DR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-DRNM
    MOVE GA-CASH TO W-GL-CR
    MOVE "CASH ON HAND" TO W-GL-CRNM
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    IF PR-AMOUNT <= 0
        MOVE 12 TO PY-RC
        MOVE "WITHDRAWAL AMOUNT MUST BE GREATER THAN ZERO" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD
    SUBTRACT PR-AMOUNT FROM W-OLD
    MOVE W-OLD TO W-NEW
    IF W-NEW < 0
        IF AC-OD-LIMIT <= 0
            PERFORM ASSESS-NSF
            EXIT PARAGRAPH
        END-IF
        IF 0 - W-NEW > AC-OD-LIMIT
            PERFORM ASSESS-NSF
            EXIT PARAGRAPH
        END-IF
    END-IF
    MOVE PR-AMOUNT TO W-AMT
    MOVE "D" TO W-DR
    MOVE "CASH WITHDRAWAL" TO PR-DESCR
    IF W-NEW < 0
        MOVE "WITHDRAWAL ACCEPTED - DRAWN ON OVERDRAFT" TO PY-MSG
    ELSE
        MOVE "WITHDRAWAL ACCEPTED" TO PY-MSG
    END-IF
    IF W-NEW >= 0 AND AC-OD-LIMIT > 0
        MOVE 0 TO BA-NSF-CNT
    END-IF
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC = 0
        MOVE 1 TO PY-POSTED
    END-IF.

      *> The item is returned: the ledger does not move, the return count
      *> goes up and the charge is journalled as its own fee entry so the
      *> statement shows the customer both halves of the story.
ASSESS-NSF.
    MOVE 8 TO PY-RC
    MOVE "INSUFFICIENT FUNDS - ITEM RETURNED" TO PY-MSG
    MOVE BA-AVAIL TO PY-OLD-BAL
    MOVE BA-AVAIL TO PY-NEW-BAL
    ADD 1 TO BA-NSF-CNT
    REWRITE BAL-REC
    PERFORM NEXT-AUDSEQ
    MOVE W-AUDSEQ TO AU-SEQ
    MOVE W-DT TO AU-DATE
    MOVE W-TM TO AU-TIME
    MOVE PR-PROG TO AU-PROG
    MOVE PR-USER TO AU-USER
    MOVE PR-TERM TO AU-TERMINAL
    MOVE "F" TO AU-ACTION
    MOVE PR-ACCT TO AU-KEY
    MOVE W-NSF-FEE TO AU-AMOUNT
    MOVE BA-AVAIL TO AU-OLD-BAL
    MOVE BA-AVAIL TO AU-NEW-BAL
    MOVE "NSF CHARGE ASSESSED - ITEM RETURNED" TO AU-MSG
    WRITE AUD-REC
    MOVE W-NSF-FEE TO PY-FEE-CHG.

      *> ==== TRANSFER ===================================================
      *> Both accounts are read and checked before either leg is written, so
      *> a rejected destination never leaves the debit applied. Because one
      *> FD holds one record, the first leg's images are parked in W-ACCT-A
      *> and W-BAL-A while the second account is read.
POST-TRANSFER.
    IF PR-ACCT = PR-ACCT2
        MOVE 48 TO PY-RC
        MOVE "TRANSFER TO AND FROM THE SAME ACCOUNT" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF PR-AMOUNT <= 0
        MOVE 12 TO PY-RC
        MOVE "TRANSFER AMOUNT MUST BE GREATER THAN ZERO" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
      *> ---- leg one: check the funds -----------------------------------
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    MOVE PR-ACCT TO W-LEG-KEY
    MOVE AC-CUST-ID TO W-LEG-CUST
    MOVE BA-AVAIL TO W-OLD
    SUBTRACT PR-AMOUNT FROM W-OLD
    IF W-OLD < 0
        IF AC-OD-LIMIT <= 0
            MOVE 8 TO PY-RC
            MOVE "INSUFFICIENT FUNDS FOR TRANSFER" TO PY-MSG
            EXIT PARAGRAPH
        END-IF
        IF 0 - W-OLD > AC-OD-LIMIT
            MOVE 8 TO PY-RC
            MOVE "INSUFFICIENT FUNDS FOR TRANSFER" TO PY-MSG
            EXIT PARAGRAPH
        END-IF
    END-IF
    MOVE W-OLD TO W-NEW
      *> ---- leg two: check before anything is written -------------------
    MOVE PR-ACCT2 TO AC-NO
    READ ACCT-FILE
    IF FS-ACCT = "23"
        MOVE 4 TO PY-RC
        MOVE "RECEIVING ACCOUNT NOT FOUND" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF FS-ACCT NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR READING RECEIVING ACCOUNT" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF AC-STATUS NOT = "A"
        MOVE 16 TO PY-RC
        MOVE "RECEIVING ACCOUNT NOT ACTIVE" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE AC-NO TO BA-NO
    READ BAL-FILE
    IF FS-BAL NOT = "00"
        MOVE 44 TO PY-RC
        MOVE "NO BALANCE ON RECEIVING ACCOUNT" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD2
    ADD PR-AMOUNT TO W-OLD2
    MOVE W-OLD2 TO W-NEW2
      *> ---- write the debit --------------------------------------------
    MOVE W-LEG-KEY TO W-LEG-KEY2
    MOVE GA-DEP TO W-GL-DR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-DRNM
    MOVE GA-DEP TO W-GL-CR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-CRNM
    PERFORM LOAD-KEY
    MOVE W-OLD TO W-NEW
    MOVE PR-AMOUNT TO W-AMT
    MOVE "D" TO W-DR
    MOVE "TRANSFER OUT" TO PR-DESCR
    MOVE W-LEG-KEY2 TO PR-ACCT
    MOVE "TRANSFER ACCEPTED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
      *> ---- write the credit -------------------------------------------
    MOVE PR-ACCT2 TO W-LEG-KEY
    PERFORM LOAD-KEY
    MOVE W-NEW2 TO W-NEW
    MOVE W-OLD2 TO W-OLD
    MOVE PR-AMOUNT TO W-AMT
    MOVE "C" TO W-DR
    MOVE TT-XFR TO PR-TYPE
    MOVE "TRANSFER IN" TO PR-DESCR
    MOVE W-LEG-KEY TO PR-ACCT
    MOVE "TRANSFER ACCEPTED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    MOVE W-LEG-KEY2 TO PR-ACCT
    IF PY-RC NOT = 0
        MOVE 40 TO PY-RC
        MOVE "TRANSFER DEBIT APPLIED BUT CREDIT FAILED" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE 2 TO PY-POSTED
    MOVE W-NEW TO PY-NEW-BAL
    MOVE W-OLD TO PY-OLD-BAL.

      *> ==== plain credit, used for interest ============================
POST-CREDIT.
    MOVE GA-INTEXP TO W-GL-DR
    MOVE "INTEREST EXPENSE" TO W-GL-DRNM
    MOVE GA-DEP TO W-GL-CR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-CRNM
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD
    ADD PR-AMOUNT TO W-OLD
    MOVE W-OLD TO W-NEW
    MOVE PR-AMOUNT TO W-AMT
    MOVE "C" TO W-DR
    IF PR-DESCR = SPACES
        MOVE "INTEREST CREDIT" TO PR-DESCR
    END-IF
    ADD PR-AMOUNT TO BA-INT-ACCRUED
    MOVE "INTEREST CREDITED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC = 0
        MOVE 1 TO PY-POSTED
    END-IF.

      *> ==== fee, allowed to overdraw the account =======================
POST-FEE.
    MOVE GA-DEP TO W-GL-DR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-DRNM
    MOVE GA-FEEINC TO W-GL-CR
    MOVE "SERVICE FEE INCOME" TO W-GL-CRNM
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    IF PR-AMOUNT <= 0
        MOVE 12 TO PY-RC
        MOVE "FEE AMOUNT MUST BE GREATER THAN ZERO" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD
    SUBTRACT PR-AMOUNT FROM W-OLD
    MOVE W-OLD TO W-NEW
    MOVE PR-AMOUNT TO W-AMT
    MOVE "D" TO W-DR
    ADD 1 TO BA-FEE-CNT
    IF PR-DESCR = SPACES
        MOVE "SERVICE FEE" TO PR-DESCR
    END-IF
    MOVE "FEE ASSESSED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC = 0
        MOVE 1 TO PY-POSTED
        MOVE PR-AMOUNT TO PY-FEE-CHG
    END-IF.

POST-NSF.
    MOVE "RETURNED ITEM CHARGE" TO PR-DESCR
    PERFORM POST-FEE.

      *> ==== loan disbursement ==========================================
POST-DISBURSE.
    MOVE GA-LOAN TO W-GL-DR
    MOVE "LOANS RECEIVABLE" TO W-GL-DRNM
    MOVE GA-DEP TO W-GL-CR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-CRNM
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    MOVE BA-AVAIL TO W-OLD
    ADD PR-AMOUNT TO W-OLD
    MOVE W-OLD TO W-NEW
    MOVE PR-AMOUNT TO W-AMT
    MOVE "C" TO W-DR
    MOVE "LOAN DISBURSEMENT" TO PR-DESCR
    MOVE "LOAN PROCEEDS CREDITED" TO PY-MSG
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC = 0
        MOVE 1 TO PY-POSTED
    END-IF.

      *> ==== loan payment ===============================================
      *> Debits the account, then applies the payment to accrued interest
      *> first and the remainder to principal, which is the standard
      *> application order.
POST-LOANPAY.
    MOVE GA-DEP TO W-GL-DR
    MOVE "CUSTOMER DEPOSITS" TO W-GL-DRNM
    MOVE GA-LOAN TO W-GL-CR
    MOVE "LOANS RECEIVABLE" TO W-GL-CRNM
    MOVE PR-LOANNO TO LN-NO
    READ LOAN-FILE
    IF FS-LOAN = "23"
        MOVE 32 TO PY-RC
        MOVE "LOAN NOT FOUND" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF FS-LOAN NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR READING LOAN" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF LN-STATUS NOT = "A"
        MOVE 16 TO PY-RC
        MOVE "LOAN IS NOT ACTIVE" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE PR-LOANNO TO LB-NO
    READ LNBAL-FILE
    IF FS-LNBAL NOT = "00"
        MOVE 32 TO PY-RC
        MOVE "NO LOAN BALANCE RECORD" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    IF PR-AMOUNT <= 0
        MOVE 12 TO PY-RC
        MOVE "PAYMENT AMOUNT MUST BE GREATER THAN ZERO" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    PERFORM READ-ACCOUNT
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    MOVE PR-AMOUNT TO W-PMTOT
    MOVE BA-AVAIL TO W-OLD
    SUBTRACT W-PMTOT FROM W-OLD
    IF W-OLD < 0
        IF AC-OD-LIMIT <= 0
            MOVE 8 TO PY-RC
            MOVE "INSUFFICIENT FUNDS FOR LOAN PAYMENT" TO PY-MSG
            EXIT PARAGRAPH
        END-IF
        IF 0 - W-OLD > AC-OD-LIMIT
            MOVE 8 TO PY-RC
            MOVE "INSUFFICIENT FUNDS FOR LOAN PAYMENT" TO PY-MSG
            EXIT PARAGRAPH
        END-IF
    END-IF
    MOVE W-OLD TO W-NEW
      *> ---- application order: interest first, then principal ----------
      *> the nightly job accrues into LB-CUR-INT, so the interest due is
      *> whatever is standing there, capped by what was actually paid
    MOVE LB-CUR-INT TO W-INTAMT
    IF W-INTAMT > W-PMTOT
        MOVE W-PMTOT TO W-INTAMT
    END-IF
    COMPUTE W-PRIN = W-PMTOT - W-INTAMT
    IF W-PRIN > LB-CUR-PRIN
        MOVE LB-CUR-PRIN TO W-PRIN
    END-IF
      *> ---- debit the account -----------------------------------------
    MOVE W-PMTOT TO W-AMT
    MOVE "D" TO W-DR
    MOVE "LOAN PAYMENT" TO PR-DESCR
    PERFORM SAVE-AND-JOURNAL
    IF PY-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
      *> ---- reduce the loan -------------------------------------------
    SUBTRACT W-INTAMT FROM LB-CUR-INT
    IF LB-CUR-INT < 0
        MOVE 0 TO LB-CUR-INT
    END-IF
    SUBTRACT W-PRIN FROM LB-CUR-PRIN
    IF LB-CUR-PRIN < 0
        MOVE 0 TO LB-CUR-PRIN
    END-IF
    ADD W-INTAMT TO LB-PAID-INT
    ADD W-PRIN TO LB-PAID-PRIN
    ADD 1 TO LB-PMTS-MADE
    MOVE W-DT TO LB-LAST-PMNT-DT
    IF LB-CUR-PRIN = 0
        MOVE "P" TO LB-DELINQ
        MOVE 0 TO LB-DAYS-PAST-DUE
    ELSE
        MOVE 0 TO LB-DAYS-PAST-DUE
        IF LB-DELINQ NOT = "0"
            MOVE "0" TO LB-DELINQ
        END-IF
    END-IF
    REWRITE LNBAL-REC
    IF FS-LNBAL NOT = "00"
        MOVE 40 TO PY-RC
        MOVE "I/O ERROR UPDATING LOAN BALANCE" TO PY-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE W-DT TO LN-LAST-PMNT-DT
    REWRITE LOAN-REC
      *> ---- tell the operator the split --------------------------------
    MOVE W-INTAMT TO W-INTED
    MOVE W-PRIN TO W-PRINED
    MOVE SPACES TO PY-MSG
    STRING "PAYMENT ACCEPTED - INTEREST " DELIMITED BY SIZE
           W-INTED      DELIMITED BY SIZE
           " PRINCIPAL " DELIMITED BY SIZE
           W-PRINED     DELIMITED BY SIZE
           INTO PY-MSG
    MOVE W-PMTOT TO PY-FEE-CHG
    MOVE 1 TO PY-POSTED.
