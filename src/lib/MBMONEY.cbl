>>SOURCE FORMAT FREE
      *> ====================================================================
      *> MBMONEY - Money field service.
      *> Two jobs: move validated text in and out of packed decimal, and do
      *> exact fixed-point arithmetic. Every balance in this bank is COMP-3,
      *> so no amount is ever carried in a binary floating register.
      *>
      *> COMMAREA dispatch: fill MONEYCOMM, CALL "MBMONEY", read MONEYCOMM.
      *> See MBDATE for why "/" inside a compound COMPUTE is avoided here too.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. MBMONEY.

DATA DIVISION.
WORKING-STORAGE SECTION.

01  W-END       PIC 99 COMP VALUE 0.
01  W-P        PIC 99 COMP VALUE 0.
01  W-CH       PIC X.
01  W-SIGN     PIC X.
01  W-DEC      PIC 99 COMP VALUE 0.
01  W-DOT      PIC 99 COMP VALUE 0.
01  W-DIG      PIC X(24).
01  W-DIGL     PIC 99 COMP VALUE 0.
01  W-NDIG     PIC 99 COMP VALUE 0.
01  W-INTD     PIC 99 COMP VALUE 0.
01  W-GRP      PIC 99 COMP VALUE 0.
01  W-COMFLG   PIC 9 COMP VALUE 0.
01  W-RAW      PIC S9(15) COMP-3.
01  W-ED       PIC -(13)9.99.
01  W-NUMP     PIC X(24).
01  W-FRAC     PIC X(2).
01  W-INTP     PIC X(13).
01  W-PAD      PIC X(13).
01  W-CMP      PIC S9(4) COMP.
01  W-HI       PIC S9(17)V9(10) COMP-3.

LINKAGE SECTION.
COPY MONEYCOMM.

PROCEDURE DIVISION USING MONEYCOMM.

MAIN-PARA.
    EVALUATE MN-FUNC
        WHEN MN-FN-VALID  PERFORM DO-PARSE
        WHEN MN-FN-FMT    PERFORM DO-FMT
        WHEN MN-FN-NEG    PERFORM DO-NEG
        WHEN MN-FN-ABS    PERFORM DO-ABS
        WHEN MN-FN-ADD    PERFORM DO-ADD
        WHEN MN-FN-SUB    PERFORM DO-SUB
        WHEN MN-FN-MUL    PERFORM DO-MUL
        WHEN MN-FN-CMP    PERFORM DO-CMP
        WHEN MN-FN-CENTS  PERFORM DO-CENTS
        WHEN OTHER
            MOVE 8 TO MN-RC
            MOVE "MONEY: UNKNOWN FUNCTION" TO MN-MSG
    END-EVALUATE
    GOBACK.

      *> ==== text -> validated packed decimal ============================
      *> Accepts optional sign, digits, thousands commas and one decimal
      *> point with at most two fractional digits. Anything else is rejected
      *> rather than silently coerced, which is what a teller window needs.
DO-PARSE.
    MOVE 0 TO MN-RC
    MOVE SPACES TO MN-MSG
    MOVE 0 TO MN-OUT
    MOVE SPACES TO W-DIG
    MOVE 0 TO W-DIGL
    MOVE 0 TO W-NDIG
    MOVE 0 TO W-DEC
    MOVE 0 TO W-DOT
    MOVE 0 TO W-GRP
    MOVE 0 TO W-COMFLG
    MOVE SPACE TO W-SIGN
    COMPUTE W-END = FUNCTION LENGTH(MN-IN)
    IF W-END < 1
        MOVE 12 TO MN-RC
        MOVE "AMOUNT REQUIRED" TO MN-MSG
        EXIT PARAGRAPH
    END-IF
      *> locate the significant end, ignoring trailing spaces
    PERFORM VARYING W-P FROM W-END BY -1 UNTIL W-P < 1
        MOVE MN-IN(W-P:1) TO W-CH
        IF W-CH NOT = SPACE
            MOVE W-P TO W-END
            EXIT PERFORM
        END-IF
    END-PERFORM
    MOVE 1 TO W-P
    PERFORM UNTIL W-P > W-END
        MOVE MN-IN(W-P:1) TO W-CH
        EVALUATE W-CH
            WHEN SPACE
                CONTINUE
            WHEN ","
                IF W-GRP < 1 OR W-GRP > 3
                    MOVE 12 TO MN-RC
                    MOVE "MALFORMED THOUSANDS SEPARATOR" TO MN-MSG
                    EXIT PERFORM
                END-IF
                MOVE 0 TO W-GRP
                MOVE 1 TO W-COMFLG
            WHEN "-"
                IF W-SIGN NOT = SPACE OR W-DIGL > 0
                    MOVE 12 TO MN-RC
                    MOVE "MINUS SIGN NOT IN LEADING POSITION" TO MN-MSG
                    EXIT PERFORM
                END-IF
                MOVE "-" TO W-SIGN
            WHEN "+"
                CONTINUE
            WHEN "."
                IF W-DOT = 1
                    MOVE 12 TO MN-RC
                    MOVE "INVALID DECIMAL POINT" TO MN-MSG
                    EXIT PERFORM
                END-IF
                IF W-COMFLG = 1 AND W-GRP NOT = 3
                    MOVE 12 TO MN-RC
                    MOVE "COMMA GROUP MUST BE THREE DIGITS" TO MN-MSG
                    EXIT PERFORM
                END-IF
                MOVE 1 TO W-DOT
            WHEN "0" WHEN "1" WHEN "2" WHEN "3" WHEN "4"
            WHEN "5" WHEN "6" WHEN "7" WHEN "8" WHEN "9"
                ADD 1 TO W-NDIG
                IF W-NDIG > 15
                    MOVE 12 TO MN-RC
                    MOVE "AMOUNT TOO LARGE" TO MN-MSG
                    EXIT PERFORM
                END-IF
                IF W-DOT = 1
                    ADD 1 TO W-DEC
                    IF W-DEC > 2
                        MOVE 12 TO MN-RC
                        MOVE "MAXIMUM 2 DECIMAL PLACES" TO MN-MSG
                        EXIT PERFORM
                    END-IF
                END-IF
                ADD 1 TO W-DIGL
                ADD 1 TO W-GRP
                MOVE W-CH TO W-DIG(W-DIGL:1)
            WHEN OTHER
                MOVE 12 TO MN-RC
                MOVE "NON NUMERIC CHARACTER IN AMOUNT" TO MN-MSG
                EXIT PERFORM
        END-EVALUATE
        ADD 1 TO W-P
    END-PERFORM
    IF MN-RC NOT = 0
        EXIT PARAGRAPH
    END-IF
    IF W-NDIG = 0
        MOVE 12 TO MN-RC
        MOVE "AMOUNT REQUIRED" TO MN-MSG
        EXIT PARAGRAPH
    END-IF
    IF W-COMFLG = 1 AND W-DOT = 0 AND W-GRP NOT = 3
        MOVE 12 TO MN-RC
        MOVE "COMMA GROUP MUST BE THREE DIGITS" TO MN-MSG
        EXIT PARAGRAPH
    END-IF
      *> pad the fraction to exactly two digits
    COMPUTE W-INTD = W-DIGL - W-DEC
    MOVE "0" TO W-FRAC
    MOVE "0" TO W-FRAC(2:1)
    IF W-DEC = 1
        MOVE W-DIG(W-DIGL:1) TO W-FRAC(1:1)
    END-IF
    IF W-DEC = 2
        MOVE W-DIG(W-DIGL - 1:2) TO W-FRAC
    END-IF
      *> pad the integer part out to thirteen digits
    MOVE SPACES TO W-PAD
    MOVE 1 TO W-P
    PERFORM UNTIL W-P > W-INTD
        MOVE W-DIG(W-P:1) TO W-PAD(W-P:1)
        ADD 1 TO W-P
    END-PERFORM
    MOVE SPACES TO W-NUMP
    MOVE W-SIGN TO W-NUMP(1:1)
    MOVE W-PAD TO W-NUMP(2:13)
    MOVE "." TO W-NUMP(15:1)
    MOVE W-FRAC TO W-NUMP(16:2)
    MOVE W-NUMP TO W-ED
    MOVE W-ED TO MN-OUT
    MOVE MN-OUT TO MN-OUT-ED.

      *> ==== packed decimal -> display =====================================
DO-FMT.
    MOVE MN-A TO MN-OUT
    MOVE MN-OUT TO MN-OUT-ED.

DO-NEG.
    COMPUTE MN-OUT = 0 - MN-A
    MOVE MN-OUT TO MN-OUT-ED.

DO-ABS.
    IF MN-A < 0
        COMPUTE MN-OUT = 0 - MN-A
    ELSE
        MOVE MN-A TO MN-OUT
    END-IF
    MOVE MN-OUT TO MN-OUT-ED.

DO-ADD.
    ADD MN-A MN-B GIVING MN-OUT
    MOVE MN-OUT TO MN-OUT-ED.

DO-SUB.
    SUBTRACT MN-B FROM MN-A GIVING MN-OUT
    MOVE MN-OUT TO MN-OUT-ED.

DO-MUL.
    COMPUTE MN-OUT = MN-A * MN-B
    MOVE MN-OUT TO MN-OUT-ED.

DO-CMP.
    IF MN-A > MN-B
        MOVE 1 TO MN-RC
    ELSE
        IF MN-A < MN-B
            MOVE -1 TO MN-RC
        ELSE
            MOVE 0 TO MN-RC
        END-IF
    END-IF.

      *> round half-up to whole cents, the way a bank ledger rounds
DO-CENTS.
    COMPUTE W-HI = MN-A
    IF W-HI >= 0
        COMPUTE W-HI = W-HI + 0.005
    ELSE
        COMPUTE W-HI = W-HI - 0.005
    END-IF
    MOVE W-HI TO MN-OUT
    MOVE MN-OUT TO MN-OUT-ED.
