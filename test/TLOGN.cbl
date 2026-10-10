>>SOURCE FORMAT FREE
      *> ====================================================================
      *> TLOGN - Unit test for MBLOGN, the sign-on service.
      *> References were computed by hand from the teller master that
      *> bin/LOADLIB writes: TELLER01/SUPVSR level 3, TELLER02/TELLER
      *> level 2, TELLER03/CLERK level 1, TELLER09 disabled.
      *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TLOGN.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY LOGCOMM.

       01  PASS-N   PIC 99 COMP VALUE 0.
       01  FAIL-N   PIC 99 COMP VALUE 0.
       01  LBL     PIC X(44).
       01  EXPED    PIC 9(4).
       01  ACTED    PIC 9(4).

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " "
           DISPLAY " MBLOGN TESTS"
           MOVE "TELLER01 / SUPVSR      -> level 3, limit 25000" TO LBL
           MOVE 3 TO EXPED
           MOVE "TELLER01" TO LG-ID
           MOVE "SUPVSR" TO LG-PASSWD
           MOVE LG-FN-ON TO LG-FUNC
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-LEVEL TO ACTED
           PERFORM CHK-NUM
           MOVE "TELLER02 / TELLER      -> level 2, limit 5000" TO LBL
           MOVE 2 TO EXPED
           MOVE "TELLER02" TO LG-ID
           MOVE "TELLER" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-LEVEL TO ACTED
           PERFORM CHK-NUM
           MOVE "TELLER03 / CLERK       -> level 1, limit 500" TO LBL
           MOVE 1 TO EXPED
           MOVE "TELLER03" TO LG-ID
           MOVE "CLERK" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-LEVEL TO ACTED
           PERFORM CHK-NUM
           MOVE "TELLER01 / WRONGPASS   -> rejected" TO LBL
           MOVE 4 TO EXPED
           MOVE "TELLER01" TO LG-ID
           MOVE "WRONGPASS" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-RC TO ACTED
           PERFORM CHK-NUM
           MOVE "TELLER09 / GONE        -> not active" TO LBL
           MOVE 16 TO EXPED
           MOVE "TELLER09" TO LG-ID
           MOVE "GONE" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-RC TO ACTED
           PERFORM CHK-NUM
           MOVE "NOSUCHOP / X           -> unknown id" TO LBL
           MOVE 4 TO EXPED
           MOVE "NOSUCHOP" TO LG-ID
           MOVE "X" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-RC TO ACTED
           PERFORM CHK-NUM
           MOVE "blank id               -> rejected" TO LBL
           MOVE 4 TO EXPED
           MOVE SPACES TO LG-ID
           MOVE "X" TO LG-PASSWD
           CALL "MBLOGN" USING LOGCOMM
           MOVE LG-RC TO ACTED
           PERFORM CHK-NUM
           DISPLAY " "
           DISPLAY " MBLOGN TESTS  PASS=" PASS-N "  FAIL=" FAIL-N
           GOBACK.

       CHK-NUM.
           IF ACTED = EXPED
               ADD 1 TO PASS-N
               DISPLAY "   ok   " LBL
           ELSE
               ADD 1 TO FAIL-N
               DISPLAY "   FAIL " LBL " expected=" EXPED " got=" ACTED
           END-IF.
