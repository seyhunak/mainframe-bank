>>SOURCE FORMAT FREE
      *> ====================================================================
      *> MBDATE - Date arithmetic service module.
      *> YYYYMMDD packed DISPLAY throughout, as on a mainframe.
      *> COMMAREA dispatch: fill DTECOMM, CALL "MBDATE", read DTECOMM.
      *>
      *> DIVISION RULE FOR THIS SHOP: never use "/" inside a COMPUTE.
      *> GnuCOBOL rounds a division that appears inside a compound COMPUTE
      *> expression but truncates it when stepped separately, so
      *>     COMPUTE X = (A * 365) + (A / 4) - (A / 100) + D
      *> evaluates with A/100 rounded while this program means it truncated.
      *> Every integer division below therefore goes through IDIV, which uses
      *> DIVIDE ... GIVING ... REMAINDER (always truncates toward zero).
      *> That keeps leap-year and century handling exact for any year.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. MBDATE.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  W-DIN       PIC 9(8).
01  W-DOUT      PIC 9(8).
01  W-Z         PIC S9(9) COMP VALUE 0.
01  W-Z2K       PIC S9(9) COMP VALUE 0.
01  W-Y         PIC S9(9) COMP VALUE 0.
01  W-M         PIC S9(9) COMP VALUE 0.
01  W-D         PIC S9(9) COMP VALUE 0.
01  W-EA        PIC S9(9) COMP VALUE 0.
01  W-YOE       PIC S9(9) COMP VALUE 0.
01  W-DOY       PIC S9(9) COMP VALUE 0.
01  W-DOE       PIC S9(9) COMP VALUE 0.
01  W-MP        PIC S9(9) COMP VALUE 0.
01  W-LEAP      PIC 9 COMP VALUE 0.
01  W-DIM       PIC 9(2) COMP VALUE 0.
01  W-DOWT      PIC 9 COMP VALUE 0.
01  W-OUTY      PIC S9(9) COMP VALUE 0.
01  W-OUTM      PIC S9(9) COMP VALUE 0.
      *> integer division operands
01  W-A         PIC S9(18) COMP VALUE 0.
01  W-B         PIC S9(18) COMP VALUE 0.
01  W-Q         PIC S9(18) COMP VALUE 0.
01  W-R         PIC S9(18) COMP VALUE 0.
01  W-T         PIC S9(18) COMP VALUE 0.
01  W-Q4        PIC S9(18) COMP VALUE 0.
01  W-Q100      PIC S9(18) COMP VALUE 0.

LINKAGE SECTION.
COPY DATECOMM.

PROCEDURE DIVISION USING DTECOMM.

MAIN-PARA.
    EVALUATE DT-FUNC
        WHEN DF-VALID     PERFORM CHK-VALID
        WHEN DF-EOM       PERFORM GET-EOM
        WHEN DF-AMON      PERFORM ADD-MONTHS
        WHEN DF-ADAY      PERFORM ADD-DAYS
        WHEN DF-DBTW      PERFORM DAYS-BETWEEN
        WHEN DF-DOW       PERFORM GET-DOW
        WHEN DF-DOM       PERFORM GET-DOM
        WHEN DF-NXTBUS    PERFORM NEXT-BUSINESS
        WHEN DF-MYEOM     PERFORM CHK-MONTH-END
        WHEN DF-DMON      PERFORM DAYS-IN-MONTH
        WHEN DF-YRBW      PERFORM YEARS-BETWEEN
        WHEN OTHER
            MOVE 8 TO DT-RC
            MOVE "DATE: UNKNOWN FUNCTION" TO DT-FLG
    END-EVALUATE
    GOBACK.

      *> ==== truncating integer division: W-Q = W-A / W-B, W-R = residue ==
IDIV.
    DIVIDE W-A BY W-B GIVING W-Q REMAINDER W-R.

      *> ==== split W-DIN (YYYYMMDD) into W-Y W-M W-D =====================
SPLIT-DATE.
    MOVE W-DIN TO W-A
    MOVE 10000 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-Y
    MOVE W-R TO W-A
    MOVE 100 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-M
    MOVE W-R TO W-D.

      *> ==== assemble W-Y W-M W-D back into an 8 digit date ==============
BUILD-OUT.
    COMPUTE W-DOUT = (W-Y * 10000) + (W-M * 100) + W-D.

      *> ==== Gregorian leap test ==========================================
      *> A leap year divides by 4, except centuries, which must divide by 400.
      *> 1900 divides by 4 but is not a leap year; 2000 is.
IS-LEAP.
    MOVE 0 TO W-LEAP
    MOVE W-Y TO W-A
    MOVE 4 TO W-B
    PERFORM IDIV
    IF W-R NOT = 0
        EXIT PARAGRAPH
    END-IF
    MOVE W-Y TO W-A
    MOVE 100 TO W-B
    PERFORM IDIV
    IF W-R NOT = 0
        MOVE 1 TO W-LEAP
        EXIT PARAGRAPH
    END-IF
    MOVE W-Y TO W-A
    MOVE 400 TO W-B
    PERFORM IDIV
    IF W-R = 0
        MOVE 1 TO W-LEAP
    END-IF.

      *> ==== days in the month held in W-M (needs W-LEAP already set) =====
COMPUTE-DIM.
    EVALUATE TRUE
        WHEN W-M = 2
            IF W-LEAP = 1
                MOVE 29 TO W-DIM
            ELSE
                MOVE 28 TO W-DIM
            END-IF
        WHEN W-M = 4 OR W-M = 6 OR W-M = 9 OR W-M = 11
            MOVE 30 TO W-DIM
        WHEN OTHER
            MOVE 31 TO W-DIM
    END-EVALUATE.

      *> ==== exact proleptic ordinal, days since 1970-01-01 ===============
      *> Howard Hinnant's days_from_civil, expressed in truncating COBOL.
      *> Note the year is only shifted down for January and February, which
      *> is what puts the leap day at the end of the year.
TO-ORDINAL.
    IF W-M <= 2
        COMPUTE W-Y = W-Y - 1
    END-IF
    MOVE W-Y TO W-A
    MOVE 400 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-EA
    MOVE W-R TO W-YOE
    COMPUTE W-MP = W-M + 9
    IF W-M > 2
        COMPUTE W-MP = W-M - 3
    END-IF
    COMPUTE W-T = (153 * W-MP) + 2
    MOVE W-T TO W-A
    MOVE 5 TO W-B
    PERFORM IDIV
    COMPUTE W-DOY = W-Q + W-D - 1
    MOVE W-YOE TO W-A
    MOVE 4 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-Q4
    MOVE W-YOE TO W-A
    MOVE 100 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-Q100
    COMPUTE W-DOE = (W-YOE * 365) + W-Q4 - W-Q100 + W-DOY
    COMPUTE W-Z = (W-EA * 146097) + W-DOE - 719468.

      *> ==== the inverse: ordinal -> Y-M-D ===============================
FROM-ORDINAL.
    COMPUTE W-T = W-Z + 719468
    MOVE W-T TO W-A
    MOVE 146097 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-EA
    MOVE W-R TO W-DOE
    MOVE W-DOE TO W-A
    MOVE 1460 TO W-B
    PERFORM IDIV
    COMPUTE W-T = W-DOE - W-Q
    MOVE W-T TO W-A
    MOVE 36524 TO W-B
    PERFORM IDIV
    COMPUTE W-T = W-T + W-Q
    MOVE W-T TO W-A
    MOVE 146096 TO W-B
    PERFORM IDIV
    COMPUTE W-T = W-T - W-Q
    MOVE W-T TO W-A
    MOVE 365 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-YOE
    MOVE W-YOE TO W-A
    MOVE 4 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-Q4
    MOVE W-YOE TO W-A
    MOVE 100 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-Q100
    COMPUTE W-T = (365 * W-YOE) + W-Q4 - W-Q100
    COMPUTE W-DOY = W-DOE - W-T
    COMPUTE W-T = (5 * W-DOY) + 2
    MOVE W-T TO W-A
    MOVE 153 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-MP
    COMPUTE W-T = (153 * W-MP) + 2
    MOVE W-T TO W-A
    MOVE 5 TO W-B
    PERFORM IDIV
    COMPUTE W-D = W-DOY - W-Q + 1
    COMPUTE W-M = W-MP + 3
    IF W-M > 12
        SUBTRACT 12 FROM W-M
        ADD 1 TO W-YOE
    END-IF
    COMPUTE W-Y = W-YOE + (W-EA * 400).

      *> ==== ordinal of whatever sits in W-DIN, result left in W-Z =======
ORD-OF-DIN.
    PERFORM SPLIT-DATE
    PERFORM TO-ORDINAL.

      *> ---- routines -----------------------------------------------------
CHK-VALID.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    IF W-Y < 1900 OR W-Y > 2999
        MOVE 12 TO DT-RC
        MOVE "DATE: YEAR OUT OF RANGE" TO DT-FLG
        EXIT PARAGRAPH
    END-IF
    IF W-M < 1 OR W-M > 12
        MOVE 13 TO DT-RC
        MOVE "DATE: MONTH OUT OF RANGE" TO DT-FLG
        EXIT PARAGRAPH
    END-IF
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    IF W-D < 1 OR W-D > W-DIM
        MOVE 14 TO DT-RC
        MOVE "DATE: DAY OUT OF RANGE" TO DT-FLG
        EXIT PARAGRAPH
    END-IF
    MOVE 0 TO DT-RC
    MOVE "GOOD" TO DT-FLG.

GET-EOM.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    MOVE W-DIM TO W-D
    PERFORM BUILD-OUT
    MOVE W-DOUT TO DT-OUT
    MOVE "OK" TO DT-FLG.

      *> add N months, clamping the day of month (31 Jan + 1m -> 28 Feb)
ADD-MONTHS.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    COMPUTE W-T = (W-Y * 12) + (W-M - 1) + DT-IN-NUM
    MOVE W-T TO W-A
    MOVE 12 TO W-B
    PERFORM IDIV
    MOVE W-Q TO W-OUTY
    MOVE W-R TO W-OUTM
    COMPUTE W-OUTM = W-OUTM + 1
    MOVE W-OUTY TO W-Y
    MOVE W-OUTM TO W-M
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    IF W-D > W-DIM
        MOVE W-DIM TO W-D
    END-IF
    PERFORM BUILD-OUT
    MOVE W-DOUT TO DT-OUT
    MOVE "OK" TO DT-FLG.

ADD-DAYS.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM TO-ORDINAL
    COMPUTE W-Z = W-Z + DT-IN-NUM
    PERFORM FROM-ORDINAL
    PERFORM BUILD-OUT
    MOVE W-DOUT TO DT-OUT
    MOVE "OK" TO DT-FLG.

      *> contract: result is (IN2 - IN1) in days, negative when IN2 precedes
DAYS-BETWEEN.
    MOVE DT-IN1 TO W-DIN
    PERFORM ORD-OF-DIN
    MOVE W-Z TO W-Z2K
    MOVE DT-IN2 TO W-DIN
    PERFORM ORD-OF-DIN
    COMPUTE DT-OUT-NUM = W-Z - W-Z2K
    MOVE "OK" TO DT-FLG.

      *> 1 = Sunday .. 7 = Saturday. Ordinal 0 (1970-01-01) was a Thursday,
      *> so remainder 0 maps to 5 and the week wraps after Saturday.
DOW-FROM-REM.
    EVALUATE W-R
        WHEN 0 MOVE 5 TO W-DOWT
        WHEN 1 MOVE 6 TO W-DOWT
        WHEN 2 MOVE 7 TO W-DOWT
        WHEN 3 MOVE 1 TO W-DOWT
        WHEN 4 MOVE 2 TO W-DOWT
        WHEN 5 MOVE 3 TO W-DOWT
        WHEN OTHER MOVE 4 TO W-DOWT
    END-EVALUATE.

GET-DOW.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM TO-ORDINAL
    MOVE W-Z TO W-A
    MOVE 7 TO W-B
    PERFORM IDIV
    PERFORM DOW-FROM-REM
    MOVE W-DOWT TO DT-OUT-NUM.

GET-DOM.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    MOVE W-D TO DT-OUT-NUM.

DAYS-IN-MONTH.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    MOVE W-DIM TO DT-OUT-NUM.

CHK-MONTH-END.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    PERFORM IS-LEAP
    PERFORM COMPUTE-DIM
    IF W-D = W-DIM
        MOVE "YES" TO DT-FLG
    ELSE
        MOVE "NO" TO DT-FLG
    END-IF.

      *> roll forward over Saturday and Sunday
NEXT-BUSINESS.
    MOVE DT-IN1 TO DT-OUT
    PERFORM 7 TIMES
        MOVE DT-OUT TO W-DIN
        PERFORM SPLIT-DATE
        PERFORM TO-ORDINAL
        MOVE W-Z TO W-A
        MOVE 7 TO W-B
        PERFORM IDIV
        PERFORM DOW-FROM-REM
        IF W-DOWT = 1 OR W-DOWT = 7
            ADD 1 TO W-Z
            PERFORM FROM-ORDINAL
            PERFORM BUILD-OUT
            MOVE W-DOUT TO DT-OUT
        END-IF
    END-PERFORM
    MOVE "OK" TO DT-FLG.

YEARS-BETWEEN.
    MOVE DT-IN1 TO W-DIN
    PERFORM SPLIT-DATE
    MOVE W-Y TO DT-OUT-NUM
    MOVE DT-IN2 TO W-DIN
    PERFORM SPLIT-DATE
    COMPUTE DT-OUT-NUM = DT-OUT-NUM - W-Y.
