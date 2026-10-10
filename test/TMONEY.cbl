>>SOURCE FORMAT FREE
      *> ====================================================================
      *> Unit test for MBMONEY: text -> validated packed decimal.
      *> Both directions matter: a teller window must accept the formatting
      *> people actually type, and must refuse everything else rather than
      *> silently coercing a bad figure onto the ledger.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. TMONEY.

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY MONEYCOMM.

01  T-IN     PIC X(24) OCCURS 24 TIMES.
01  T-EXP    PIC S9(13)V99 COMP-3 OCCURS 24 TIMES.
01  T-MODE   PIC X OCCURS 24 TIMES.
01  T-DESC   PIC X(32) OCCURS 24 TIMES.

01  PASS-N   PIC 99 COMP VALUE 0.
01  FAIL-N   PIC 99 COMP VALUE 0.
01  I        PIC 99 COMP VALUE 0.
01  LBL      PIC X(24).
01  EXPED    PIC -Z(11)9.99.

PROCEDURE DIVISION.
MAIN.
    MOVE "0" TO T-IN(1)
    MOVE 0 TO T-EXP(1)
    MOVE "A" TO T-MODE(1)
    MOVE "PLAIN ZERO" TO T-DESC(1)
    MOVE "1" TO T-IN(2)
    MOVE 1 TO T-EXP(2)
    MOVE "A" TO T-MODE(2)
    MOVE "PLAIN ONE" TO T-DESC(2)
    MOVE "1234.56" TO T-IN(3)
    MOVE 1234.56 TO T-EXP(3)
    MOVE "A" TO T-MODE(3)
    MOVE "TWO DECIMAL PLACES" TO T-DESC(3)
    MOVE "0.05" TO T-IN(4)
    MOVE 0.05 TO T-EXP(4)
    MOVE "A" TO T-MODE(4)
    MOVE "LEADING ZERO FRACTION" TO T-DESC(4)
    MOVE ".5" TO T-IN(5)
    MOVE 0.5 TO T-EXP(5)
    MOVE "A" TO T-MODE(5)
    MOVE "NO WHOLE PART" TO T-DESC(5)
    MOVE "1234567.89" TO T-IN(6)
    MOVE 1234567.89 TO T-EXP(6)
    MOVE "A" TO T-MODE(6)
    MOVE "SEVEN DIGITS" TO T-DESC(6)
    MOVE "1,234.56" TO T-IN(7)
    MOVE 1234.56 TO T-EXP(7)
    MOVE "A" TO T-MODE(7)
    MOVE "THOUSANDS COMMA" TO T-DESC(7)
    MOVE "1,234,567.89" TO T-IN(8)
    MOVE 1234567.89 TO T-EXP(8)
    MOVE "A" TO T-MODE(8)
    MOVE "DOUBLE COMMA" TO T-DESC(8)
    MOVE "  42.50  " TO T-IN(9)
    MOVE 42.5 TO T-EXP(9)
    MOVE "A" TO T-MODE(9)
    MOVE "PADDED WITH SPACES" TO T-DESC(9)
    MOVE "000123.45" TO T-IN(10)
    MOVE 123.45 TO T-EXP(10)
    MOVE "A" TO T-MODE(10)
    MOVE "LEADING ZEROS" TO T-DESC(10)
    MOVE "-500.25" TO T-IN(11)
    MOVE -500.25 TO T-EXP(11)
    MOVE "A" TO T-MODE(11)
    MOVE "NEGATIVE AMOUNT" TO T-DESC(11)
    MOVE "100" TO T-IN(12)
    MOVE 100 TO T-EXP(12)
    MOVE "A" TO T-MODE(12)
    MOVE "INTEGER ONLY" TO T-DESC(12)
    MOVE "99999999999.99" TO T-IN(13)
    MOVE 99999999999.990005 TO T-EXP(13)
    MOVE "A" TO T-MODE(13)
    MOVE "NEAR 13 DIGIT LIMIT" TO T-DESC(13)
    MOVE "+75.10" TO T-IN(14)
    MOVE 75.1 TO T-EXP(14)
    MOVE "A" TO T-MODE(14)
    MOVE "EXPLICIT PLUS" TO T-DESC(14)
    MOVE "1e5" TO T-IN(15)
    MOVE 0 TO T-EXP(15)
    MOVE "R" TO T-MODE(15)
    MOVE "EXPONENT NOT ALLOWED" TO T-DESC(15)
    MOVE "12.3.4" TO T-IN(16)
    MOVE 0 TO T-EXP(16)
    MOVE "R" TO T-MODE(16)
    MOVE "TWO DECIMAL POINTS" TO T-DESC(16)
    MOVE "12.345" TO T-IN(17)
    MOVE 0 TO T-EXP(17)
    MOVE "R" TO T-MODE(17)
    MOVE "THREE DECIMALS" TO T-DESC(17)
    MOVE "-" TO T-IN(18)
    MOVE 0 TO T-EXP(18)
    MOVE "R" TO T-MODE(18)
    MOVE "LONE MINUS" TO T-DESC(18)
    MOVE "." TO T-IN(19)
    MOVE 0 TO T-EXP(19)
    MOVE "R" TO T-MODE(19)
    MOVE "LONE POINT" TO T-DESC(19)
    MOVE SPACES TO T-IN(20)
    MOVE 0 TO T-EXP(20)
    MOVE "R" TO T-MODE(20)
    MOVE "BLANK" TO T-DESC(20)
    MOVE "12-34" TO T-IN(21)
    MOVE 0 TO T-EXP(21)
    MOVE "R" TO T-MODE(21)
    MOVE "EMBEDDED MINUS" TO T-DESC(21)
    MOVE "1,00.00" TO T-IN(22)
    MOVE 0 TO T-EXP(22)
    MOVE "R" TO T-MODE(22)
    MOVE "BADLY PLACED COMMA" TO T-DESC(22)
    MOVE "1234567890123456" TO T-IN(23)
    MOVE 0 TO T-EXP(23)
    MOVE "R" TO T-MODE(23)
    MOVE "SIXTEEN DIGITS" TO T-DESC(23)
    MOVE "abc" TO T-IN(24)
    MOVE 0 TO T-EXP(24)
    MOVE "R" TO T-MODE(24)
    MOVE "LETTERS" TO T-DESC(24)

    MOVE 0 TO FAIL-N
    MOVE 0 TO PASS-N
    PERFORM VARYING I FROM 1 BY 1 UNTIL I > 24
        MOVE T-IN(I) TO LBL
        MOVE LBL TO MN-IN
        MOVE MN-FN-VALID TO MN-FUNC
        CALL "MBMONEY" USING MONEYCOMM
        MOVE T-EXP(I) TO EXPED
        IF T-MODE(I) = "A"
            IF MN-RC = 0 AND MN-OUT-ED = EXPED
                ADD 1 TO PASS-N
            ELSE
                ADD 1 TO FAIL-N
                DISPLAY " FAIL [" LBL "] got [" MN-OUT-ED "] rc=" MN-RC " " MN-MSG
                DISPLAY "      accepted expected [" EXPED "] : " T-DESC(I)
            END-IF
        ELSE
            IF MN-RC = 0
                ADD 1 TO FAIL-N
                DISPLAY " FAIL [" LBL "] ACCEPTED as [" MN-OUT-ED "] but must reject"
                DISPLAY "      rejected expected : " T-DESC(I)
            ELSE
                ADD 1 TO PASS-N
            END-IF
        END-IF
    END-PERFORM
    DISPLAY " "
    DISPLAY " MBMONEY TESTS  PASS=" PASS-N "  FAIL=" FAIL-N
    GOBACK.
