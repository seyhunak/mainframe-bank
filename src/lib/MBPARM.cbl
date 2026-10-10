>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBPARM - Bank parameter service.
       *>
       *> Fees, rates and limits live in PARMFILE, not in program literals,
       *> so a rate change is a data change and the nightly job can report
       *> on what the bank is actually charging.
       *>
       *> COMMAREA dispatch: fill PARMCOMM, CALL "MBPARM", read PARMCOMM.
       *>   GET   PM-NAME in, PM-VALUE / PM-TEXT out
       *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBPARM.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT PARM-FILE ASSIGN TO "data/PARMFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PRM-NAME
               FILE STATUS IS FS-PARM.

       DATA DIVISION.
       FILE SECTION.
       FD  PARM-FILE.
       COPY PARMREC.

       WORKING-STORAGE SECTION.
       COPY BANKCON.
       01  FS-PARM        PIC XX.
       01  W-DEF-BAL      PIC S9(13)V99 COMP-3 VALUE 0.

       LINKAGE SECTION.
       COPY PARMCOMM.

       PROCEDURE DIVISION USING PARMCOMM.

       MAIN-PARA.
           MOVE 0 TO PM-RC
           MOVE SPACES TO PM-MSG
           MOVE 0 TO PM-VALUE
           MOVE SPACES TO PM-TEXT
           IF PM-NAME = SPACES
               MOVE 48 TO PM-RC
               MOVE "PARM: NAME REQUIRED" TO PM-MSG
               GOBACK
           END-IF
           OPEN INPUT PARM-FILE
           IF FS-PARM NOT = "00"
               MOVE 40 TO PM-RC
               MOVE "PARM: PARAMETER DATASET UNAVAILABLE" TO PM-MSG
               GOBACK
           END-IF
           MOVE PM-NAME TO PRM-NAME
           READ PARM-FILE
           EVALUATE FS-PARM
               WHEN "00"
                   MOVE PRM-VALUE TO PM-VALUE
                   MOVE PRM-TEXT TO PM-TEXT
               WHEN "23"
                   MOVE 44 TO PM-RC
                   MOVE "PARM: UNKNOWN PARAMETER" TO PM-MSG
               WHEN OTHER
                   MOVE 40 TO PM-RC
                   MOVE "PARM: I/O ERROR READING PARAMETER" TO PM-MSG
           END-EVALUATE
           CLOSE PARM-FILE
           GOBACK.
