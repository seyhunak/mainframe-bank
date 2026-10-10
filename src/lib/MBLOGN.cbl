>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBLOGN - Sign-on service, in the shape of a CICS sign-on program.
       *>
       *> Takes the operator id and password from the commarea, checks them
       *> against the teller master, and returns the teller's authority level
       *> and limits so the terminal can set the session context. Repeated
       *> failures are counted on the teller record; three in a row and the
       *> operator is locked out, which is the behaviour a security review
       *> expects before anything else gets written.
       *>
       *> COMMAREA dispatch: fill LOGCOMM, CALL "MBLOGN", read LOGCOMM.
       *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBLOGN.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT TELLER-FILE ASSIGN TO "data/TELLFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TE-ID
               FILE STATUS IS FS-TELL.

       DATA DIVISION.
       FILE SECTION.
       FD  TELLER-FILE.
       COPY TELLREC.

       WORKING-STORAGE SECTION.
       COPY BANKCON.
       01  FS-TELL        PIC XX.
       01  W-FAILS        PIC 9(2) COMP VALUE 0.
       01  W-LOCKOUT      PIC 9(2) COMP VALUE 3.

       LINKAGE SECTION.
       COPY LOGCOMM.

       PROCEDURE DIVISION USING LOGCOMM.
       MAIN-PARA.
           MOVE 0 TO LG-RC
           MOVE SPACES TO LG-MSG
           IF LG-ID = SPACES
               MOVE 4 TO LG-RC
               MOVE "OPERATOR ID REQUIRED" TO LG-MSG
               GOBACK
           END-IF
           OPEN I-O TELLER-FILE
           IF FS-TELL NOT = "00"
               MOVE 40 TO LG-RC
               MOVE "SIGN-ON: TELLER MASTER UNAVAILABLE" TO LG-MSG
               GOBACK
           END-IF
           MOVE LG-ID TO TE-ID
           READ TELLER-FILE
           EVALUATE FS-TELL
               WHEN "00"
                   CONTINUE
               WHEN "23"
                   MOVE 4 TO LG-RC
                   MOVE "UNKNOWN OPERATOR ID" TO LG-MSG
                   CLOSE TELLER-FILE
                   GOBACK
               WHEN OTHER
                   MOVE 40 TO LG-RC
                   MOVE "SIGN-ON: I/O ERROR READING TELLER MASTER" TO LG-MSG
                   CLOSE TELLER-FILE
                   GOBACK
           END-EVALUATE
           IF TE-STATUS NOT = "A"
               MOVE 16 TO LG-RC
               MOVE "OPERATOR NOT ACTIVE - SEE SUPERVISOR" TO LG-MSG
               CLOSE TELLER-FILE
               GOBACK
           END-IF
           IF TE-PASSWD = LG-PASSWD
               PERFORM ACCEPT-ON
           ELSE
               PERFORM REJECT-ON
           END-IF
           CLOSE TELLER-FILE
           GOBACK.

       ACCEPT-ON.
           MOVE 0 TO W-FAILS
           MOVE 0 TO TE-LOCK-CNT
           REWRITE TELLER-REC
           IF FS-TELL NOT = "00"
               MOVE 40 TO LG-RC
               MOVE "SIGN-ON: CANNOT UPDATE TELLER MASTER" TO LG-MSG
               EXIT PARAGRAPH
           END-IF
           MOVE 0 TO LG-RC
           MOVE TE-NAME TO LG-NAME
           MOVE TE-BRANCH TO LG-BRANCH
           MOVE TE-LEVEL TO LG-LEVEL
           MOVE TE-LIMIT TO LG-LIMIT
           MOVE TE-CASH-LIMIT TO LG-CASH-LIMIT
           MOVE "SIGN-ON ACCEPTED" TO LG-MSG.

       REJECT-ON.
           ADD 1 TO TE-LOCK-CNT
           REWRITE TELLER-REC
           IF TE-LOCK-CNT >= W-LOCKOUT
               MOVE "X" TO TE-STATUS
               REWRITE TELLER-REC
               MOVE 16 TO LG-RC
               MOVE "OPERATOR LOCKED OUT - TOO MANY FAILURES" TO LG-MSG
           ELSE
               MOVE 4 TO LG-RC
               MOVE "PASSWORD INCORRECT" TO LG-MSG
           END-IF.
