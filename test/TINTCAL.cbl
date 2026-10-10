>>SOURCE FORMAT FREE
      *> ====================================================================
      *> Unit test for MBINTCAL.
      *> References were computed independently in Python using the standard
      *> annuity formula; a cent of disagreement here would misprice every
      *> loan the bank writes.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. TINTCAL.

DATA DIVISION.
WORKING-STORAGE SECTION.
COPY INTCOMM.

01  PASS-N   PIC 99 COMP VALUE 0.
01  FAIL-N   PIC 99 COMP VALUE 0.
01  LBL      PIC X(40).
01  EXPED    PIC -Z(11)9.99.
01  ACTED    PIC -Z(11)9.99.
01  BAL      PIC S9(13)V99 COMP-3.
01  PAY      PIC S9(13)V99 COMP-3.
01  TOTINT   PIC S9(13)V99 COMP-3.
01  PRINCIP  PIC S9(13)V99 COMP-3.
01  K        PIC 999 COMP VALUE 0.
01  NMON     PIC 999 COMP VALUE 0.
01  RATE     PIC S9(5)V9(10) COMP-3.
01  LASTPAY  PIC S9(13)V99 COMP-3.
01  EXPN     PIC S9(13)V99 COMP-3.

PROCEDURE DIVISION.
MAIN.
    MOVE 0 TO FAIL-N
    MOVE 0 TO PASS-N

      *> --- simple interest, actual/365 ---------------------------------
    MOVE "SIMPLE 10000 @5.00% for 30 days = 41.10" TO LBL
    MOVE ICF-SIMPLE TO IC-FUNC
    MOVE 10000 TO IC-AMT1
    MOVE 0.05 TO IC-RATE
    MOVE 30 TO IC-N
    PERFORM CALL-INT
    MOVE 41.10 TO EXPED
    PERFORM CHK

    MOVE "SIMPLE 10000 @5.00% for 365 days = 500.00" TO LBL
    MOVE ICF-SIMPLE TO IC-FUNC
    MOVE 10000 TO IC-AMT1
    MOVE 0.05 TO IC-RATE
    MOVE 365 TO IC-N
    PERFORM CALL-INT
    MOVE 500.00 TO EXPED
    PERFORM CHK

      *> --- savings, minimum balance rule --------------------------------
    MOVE "SAVINGS 5000 @4.00% 30d min 1000 = 16.44" TO LBL
    MOVE ICF-SAVTIER TO IC-FUNC
    MOVE 5000 TO IC-AMT1
    MOVE 30 TO IC-AMT2
    MOVE 1000 TO IC-AMT3
    MOVE 0.04 TO IC-RATE
    PERFORM CALL-INT
    MOVE 16.44 TO EXPED
    PERFORM CHK

    MOVE "SAVINGS 900 @4.00% 30d min 1000 = 0.00" TO LBL
    MOVE ICF-SAVTIER TO IC-FUNC
    MOVE 900 TO IC-AMT1
    MOVE 30 TO IC-AMT2
    MOVE 1000 TO IC-AMT3
    MOVE 0.04 TO IC-RATE
    PERFORM CALL-INT
    MOVE 0.00 TO EXPED
    PERFORM CHK

      *> --- monthly payment ----------------------------------------------
    MOVE "AMORT 200000 @6.50% 360mo = 1264.14" TO LBL
    MOVE ICF-PAYMENT TO IC-FUNC
    MOVE 200000 TO IC-AMT1
    MOVE 0.065 TO IC-RATE
    MOVE 360 TO IC-N
    PERFORM CALL-INT
    MOVE 1264.14 TO EXPED
    PERFORM CHK

    MOVE "AMORT 25000 @4.90% 60mo = 470.64" TO LBL
    MOVE ICF-PAYMENT TO IC-FUNC
    MOVE 25000 TO IC-AMT1
    MOVE 0.049 TO IC-RATE
    MOVE 60 TO IC-N
    PERFORM CALL-INT
    MOVE 470.64 TO EXPED
    PERFORM CHK

    MOVE "AMORT 120000 @7.25% 180mo = 1095.44" TO LBL
    MOVE ICF-PAYMENT TO IC-FUNC
    MOVE 120000 TO IC-AMT1
    MOVE 0.0725 TO IC-RATE
    MOVE 180 TO IC-N
    PERFORM CALL-INT
    MOVE 1095.44 TO EXPED
    PERFORM CHK

    MOVE "AMORT 5000 @0.00% 12mo = 416.67" TO LBL
    MOVE ICF-PAYMENT TO IC-FUNC
    MOVE 5000 TO IC-AMT1
    MOVE 0 TO IC-RATE
    MOVE 12 TO IC-N
    PERFORM CALL-INT
    MOVE 416.67 TO EXPED
    PERFORM CHK

      *> --- walk a 12 month loan to zero ---------------------------------
      *> 12000 at 12% over 12 months. Each step applies the constant payment;
      *> the final payment absorbs the rounding residue so the balance
      *> lands exactly on zero, which is what a servicing system must do.
    MOVE 12000 TO BAL
    MOVE 0.12 TO RATE
    MOVE 12 TO NMON
    MOVE ICF-PAYMENT TO IC-FUNC
    MOVE 12000 TO IC-AMT1
    MOVE RATE TO IC-RATE
    MOVE NMON TO IC-N
    PERFORM CALL-INT
    MOVE IC-OUT TO PAY
    MOVE 0 TO TOTINT
    MOVE 0 TO LASTPAY
    PERFORM VARYING K FROM 1 BY 1 UNTIL K > NMON
        MOVE ICF-STEP TO IC-FUNC
        MOVE BAL TO IC-AMT1
        MOVE PAY TO IC-AMT2
        MOVE RATE TO IC-RATE
        PERFORM CALL-INT
        ADD IC-OUT TO TOTINT
        MOVE IC-OUT3 TO BAL
        IF K = NMON
            MOVE IC-AMT2 TO LASTPAY
        END-IF
    END-PERFORM
    MOVE "AMORT walk 12000@12%/12mo ends at 0.00" TO LBL
    IF BAL = 0
        ADD 1 TO PASS-N
    ELSE
        ADD 1 TO FAIL-N
        DISPLAY " FAIL " LBL
        DISPLAY "      closing balance is not zero"
    END-IF
    MOVE "AMORT walk last payment absorbs residue" TO LBL
    MOVE TOTINT TO EXPED
    IF LASTPAY >= PAY
        ADD 1 TO PASS-N
    ELSE
        ADD 1 TO FAIL-N
        DISPLAY " FAIL last payment " ACTED " < regular " EXPED
    END-IF
    MOVE PAY TO ACTED
    DISPLAY "  walk detail:"
    DISPLAY "    payment       [" ACTED "]"
    MOVE TOTINT TO ACTED
    DISPLAY "    total interest[" ACTED "]"
    MOVE LASTPAY TO ACTED
    DISPLAY "    final payment [" ACTED "]"
    MOVE BAL TO ACTED
    DISPLAY "    closing bal   [" ACTED "]"
    MOVE 0 TO EXPED
    MOVE 794.23 TO EXPED

    MOVE "AMORT walk total interest = 794.23" TO LBL
    MOVE TOTINT TO ACTED
    MOVE 794.23 TO EXPN
    IF TOTINT = EXPN
        ADD 1 TO PASS-N
    ELSE
        ADD 1 TO FAIL-N
        DISPLAY " FAIL " LBL
        DISPLAY "      got [" ACTED "] want [" EXPED "]"
    END-IF
    DISPLAY " "
    DISPLAY " MBINTCAL TESTS  PASS=" PASS-N "  FAIL=" FAIL-N
    GOBACK.

CALL-INT.
    CALL "MBINTCAL" USING INTCOMM.

CHK.
    MOVE IC-OUT TO ACTED
    IF IC-RC = 0 AND ACTED = EXPED
        ADD 1 TO PASS-N
    ELSE
        ADD 1 TO FAIL-N
        DISPLAY " FAIL " LBL
        DISPLAY "      got [" ACTED "] rc=" IC-RC " want [" EXPED "]"
    END-IF.
