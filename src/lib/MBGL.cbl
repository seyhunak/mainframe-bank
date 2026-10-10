>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBGL - General ledger balance maintenance.
       *>
       *> MBPOSTNG decides *which* accounts a posting touches; this module
       *> applies the amounts to the control account balances so a nightly
       *> proof can check that the ledger is in balance without reading the
       *> whole entry file. Called inside the posting engine, in the same
       *> task, so a balance and its ledger side can never disagree.
       *>
       *> COMMAREA dispatch: fill GLCOMM, CALL "MBGL", read GLCOMM.
       *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBGL.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT GLB-FILE ASSIGN TO "data/GLBFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GB-ACCT
               FILE STATUS IS FS-GLB.

       DATA DIVISION.
       FILE SECTION.
       FD  GLB-FILE.
       COPY GLBREC.

       WORKING-STORAGE SECTION.
       COPY BANKCON.
       01  FS-GLB         PIC XX.

       LINKAGE SECTION.
       COPY GLCOMM.

       PROCEDURE DIVISION USING GLCOMM.

       MAIN-PARA.
           MOVE 0 TO GC-RC
           MOVE SPACES TO GC-MSG
           OPEN I-O GLB-FILE
           IF FS-GLB NOT = "00"
               MOVE 40 TO GC-RC
               MOVE "GL: CONTROL DATASET UNAVAILABLE" TO GC-MSG
               GOBACK
           END-IF
           PERFORM APPLY-SIDE
           CLOSE GLB-FILE
           GOBACK.

       APPLY-SIDE.
           IF GC-ACCT = SPACES
               MOVE 40 TO GC-RC
               MOVE "GL: BLANK ACCOUNT CODE" TO GC-MSG
               EXIT PARAGRAPH
           END-IF
           MOVE GC-ACCT TO GB-ACCT
           READ GLB-FILE
           EVALUATE FS-GLB
               WHEN "00" CONTINUE
               WHEN "23"
                   MOVE SPACES TO GLB-REC
                   MOVE GC-ACCT TO GB-ACCT
                   MOVE GC-NAME TO GB-NAME
                   MOVE 0 TO GB-DEBITS GB-CREDITS GB-BALANCE
                   WRITE GLB-REC
                   IF FS-GLB NOT = "00"
                       MOVE 40 TO GC-RC
                       MOVE "GL: CANNOT CREATE CONTROL ACCOUNT" TO GC-MSG
                       EXIT PARAGRAPH
                   END-IF
               WHEN OTHER
                   MOVE 40 TO GC-RC
                   MOVE "GL: I/O ERROR READING CONTROL ACCOUNT" TO GC-MSG
                   EXIT PARAGRAPH
           END-EVALUATE
           IF GC-SIDE = "C"
               ADD GC-AMOUNT TO GB-CREDITS
               SUBTRACT GC-AMOUNT FROM GB-BALANCE
           ELSE
               ADD GC-AMOUNT TO GB-DEBITS
               ADD GC-AMOUNT TO GB-BALANCE
           END-IF
           REWRITE GLB-REC
           IF FS-GLB NOT = "00"
               MOVE 40 TO GC-RC
               MOVE "GL: I/O ERROR UPDATING CONTROL ACCOUNT" TO GC-MSG
           END-IF.
