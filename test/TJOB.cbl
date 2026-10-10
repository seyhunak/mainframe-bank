>>SOURCE FORMAT FREE
      *> ====================================================================
      *> TJOB - Unit test for MBJB001, the night-end batch job.
      *> Reads the job log the batch leaves behind and checks the counts it
      *> reports against what the datasets actually hold: four loans, four
      *> interest postings, one statement line per journal entry, six control
      *> accounts, and a ledger that nets to zero.
      *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TJOB.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT JOBLOG-FILE ASSIGN TO "data/JOBLOG"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JOB.
           SELECT TXN-FILE ASSIGN TO "data/TXNFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS SEQUENTIAL
               RECORD KEY IS TX-SEQ
               FILE STATUS IS FS-TXN.
           SELECT GLB-FILE ASSIGN TO "data/GLBFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS DYNAMIC
               RECORD KEY IS GB-ACCT
               FILE STATUS IS FS-GLB.

       DATA DIVISION.
       FILE SECTION.
       FD  JOBLOG-FILE.
       COPY JOBREC.
       FD  TXN-FILE.
       COPY TXNREC.
       FD  GLB-FILE.
       COPY GLBREC.

       WORKING-STORAGE SECTION.
       01  FS-JOB      PIC XX.
       01  FS-TXN      PIC XX.
       01  FS-GLB      PIC XX.
       01  PASS-N      PIC 99 COMP VALUE 0.
       01  FAIL-N      PIC 99 COMP VALUE 0.
       01  LBL        PIC X(44).
       01  EXPED       PIC 9(6).
       01  ACTED       PIC 9(6).
       01  W-LAST-JOB  PIC X(10).
       01  W-TXN-N     PIC 9(6) VALUE 0.
       01  W-GL-N      PIC 9(6) VALUE 0.
       01  W-GL-BAL    PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-EOF       PIC X VALUE "N".

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " "
           DISPLAY " MBJB001 TESTS"
           PERFORM READ-JOBLOG
           PERFORM COUNT-JOURNAL
           PERFORM COUNT-LEDGER
           MOVE "last job was NIGHT-END" TO LBL
           MOVE 1 TO EXPED
           IF W-LAST-JOB = "NIGHT-END"
               MOVE 1 TO ACTED
           ELSE
               MOVE 0 TO ACTED
           END-IF
           PERFORM CHK-NUM
           MOVE "loans scanned = 4" TO LBL
           MOVE 4 TO EXPED
           MOVE JO-LOANS TO ACTED
           PERFORM CHK-NUM
           MOVE "interest posted = 4" TO LBL
           MOVE 4 TO EXPED
           MOVE JO-INTACCR TO ACTED
           PERFORM CHK-NUM
           MOVE "statement lines = journal entries" TO LBL
           MOVE W-TXN-N TO EXPED
           MOVE JO-STATMNT TO ACTED
           PERFORM CHK-NUM
           MOVE "control accounts = 6" TO LBL
           MOVE 6 TO EXPED
           MOVE JO-GLACCTS TO ACTED
           PERFORM CHK-NUM
           MOVE "ledger nets to zero" TO LBL
           MOVE 0 TO EXPED
           MOVE JO-GLBAL TO ACTED
           PERFORM CHK-NUM
           DISPLAY " "
           DISPLAY " MBJB001 TESTS  PASS=" PASS-N "  FAIL=" FAIL-N
           GOBACK.

       READ-JOBLOG.
           OPEN INPUT JOBLOG-FILE
           IF FS-JOB NOT = "00"
               DISPLAY "   cannot open job log, FS=" FS-JOB
               CLOSE JOBLOG-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE "N" TO W-EOF
           PERFORM UNTIL W-EOF = "Y"
               READ JOBLOG-FILE
                   AT END MOVE "Y" TO W-EOF
                   NOT AT END MOVE JO-JOB TO W-LAST-JOB
               END-READ
           END-PERFORM
           CLOSE JOBLOG-FILE.

       COUNT-JOURNAL.
           OPEN INPUT TXN-FILE
           IF FS-TXN NOT = "00"
               DISPLAY "   cannot open journal, FS=" FS-TXN
               CLOSE TXN-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE "N" TO W-EOF
           PERFORM UNTIL W-EOF = "Y"
               READ TXN-FILE NEXT RECORD
                   AT END MOVE "Y" TO W-EOF
                   NOT AT END ADD 1 TO W-TXN-N
               END-READ
           END-PERFORM
           CLOSE TXN-FILE.

       COUNT-LEDGER.
           OPEN INPUT GLB-FILE
           IF FS-GLB NOT = "00"
               DISPLAY "   cannot open ledger balances, FS=" FS-GLB
               CLOSE GLB-FILE
               EXIT PARAGRAPH
           END-IF
           MOVE "N" TO W-EOF
           PERFORM UNTIL W-EOF = "Y"
               READ GLB-FILE NEXT RECORD
                   AT END
                       MOVE "Y" TO W-EOF
                   NOT AT END
                       IF FS-GLB = "00"
                           ADD 1 TO W-GL-N
                           ADD GB-BALANCE TO W-GL-BAL
                       ELSE
                           DISPLAY "   GLB read FS=" FS-GLB
                           MOVE "Y" TO W-EOF
                       END-IF
               END-READ
           END-PERFORM
           CLOSE GLB-FILE.

       CHK-NUM.
           IF ACTED = EXPED
               ADD 1 TO PASS-N
               DISPLAY "   ok   " LBL
           ELSE
               ADD 1 TO FAIL-N
               DISPLAY "   FAIL " LBL " expected=" EXPED " got=" ACTED
           END-IF.
