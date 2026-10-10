>>SOURCE FORMAT FREE
      *> ====================================================================
      *> TPARM - Unit test for MBPARM, the parameter service.
      *> References are the records bin/LOADLIB writes into PARMFILE.
      *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. TPARM.

       DATA DIVISION.
       WORKING-STORAGE SECTION.
       COPY PARMCOMM.

       01  PASS-N   PIC 99 COMP VALUE 0.
       01  FAIL-N   PIC 99 COMP VALUE 0.
       01  LBL     PIC X(40).
       01  EXPED    PIC -Z(9)9.99.
       01  ACTED    PIC -Z(9)9.99.

       PROCEDURE DIVISION.
       MAIN.
           DISPLAY " "
           DISPLAY " MBPARM TESTS"
           MOVE "NSF-FEE        = 25.00" TO LBL
           MOVE 25.00 TO EXPED
           MOVE "NSF-FEE" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-VALUE TO ACTED
           PERFORM CHK-NUM
           MOVE "MINBAL-S       = 500.00" TO LBL
           MOVE 500.00 TO EXPED
           MOVE "MINBAL-S" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-VALUE TO ACTED
           PERFORM CHK-NUM
           MOVE "RATE-S         = 0.0425" TO LBL
           MOVE 0.0425 TO EXPED
           MOVE "RATE-S" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-VALUE TO ACTED
           PERFORM CHK-NUM
           MOVE "ODLIM-S        = 0.00" TO LBL
           MOVE 0 TO EXPED
           MOVE "ODLIM-S" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-VALUE TO ACTED
           PERFORM CHK-NUM
           MOVE "LOCK-MAX-MIN   = 5" TO LBL
           MOVE 5 TO EXPED
           MOVE "LOCK-MAX-MIN" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-VALUE TO ACTED
           PERFORM CHK-NUM
           MOVE "NOSUCHPARM     -> rc 44" TO LBL
           MOVE 44 TO EXPED
           MOVE "NOSUCHPARM" TO PM-NAME
           CALL "MBPARM" USING PARMCOMM
           MOVE PM-RC TO ACTED
           PERFORM CHK-NUM
           DISPLAY " "
           DISPLAY " MBPARM TESTS  PASS=" PASS-N "  FAIL=" FAIL-N
           GOBACK.

       CHK-NUM.
           IF ACTED = EXPED
               ADD 1 TO PASS-N
               DISPLAY "   ok   " LBL
           ELSE
               ADD 1 TO FAIL-N
               DISPLAY "   FAIL " LBL " expected=" EXPED " got=" ACTED
           END-IF.
