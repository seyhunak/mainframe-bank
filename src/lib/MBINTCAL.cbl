>>SOURCE FORMAT FREE
      *> ====================================================================
      *> MBINTCAL - Interest and loan amortisation engine.
      *>
      *> All arithmetic stays in packed decimal. The annuity factor needed by
      *> AMOR is accumulated by summation with truncating decimal division
      *> rather than by a "**" power, because a binary float would put the
      *> monthly payment out by a cent on long terms.
      *>
      *> COMMAREA dispatch: fill INTCOMM, CALL "MBINTCAL", read INTCOMM.
      *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. MBINTCAL.

DATA DIVISION.
WORKING-STORAGE SECTION.
01  W-NA       PIC S9(18)V9(10) COMP-3.
01  W-NB       PIC S9(18)V9(10) COMP-3.
01  W-NQ       PIC S9(18)V9(10) COMP-3.
01  W-NR       PIC S9(18)V9(10) COMP-3.
01  W-MRATE    PIC S9(5)V9(10) COMP-3.
01  W-DISC     PIC S9(18)V9(10) COMP-3.
01  W-AF       PIC S9(18)V9(10) COMP-3.
01  W-TEMP     PIC S9(18)V9(10) COMP-3.
01  W-TEMP2    PIC S9(18)V9(10) COMP-3.
01  W-I        PIC S9(9) COMP-3.
01  W-NTERM    PIC S9(9) COMP-3.
01  W-BASIS    PIC S9(9) COMP-3.
01  W-SAVPRIN  PIC S9(13)V99 COMP-3.
01  W-SBAL     PIC S9(18)V9(10) COMP-3.
01  W-SRATE    PIC S9(5)V9(10) COMP-3.
01  W-SDAYS    PIC S9(9) COMP-3.
01  W-SBASIS   PIC S9(9) COMP-3.

LINKAGE SECTION.
COPY INTCOMM.

PROCEDURE DIVISION USING INTCOMM.

MAIN-PARA.
    MOVE 0 TO IC-RC
    MOVE SPACES TO IC-MSG
    EVALUATE IC-FUNC
        WHEN ICF-SIMPLE    PERFORM DO-SIMPLE
        WHEN ICF-SAVTIER   PERFORM DO-SAVINGS
        WHEN ICF-PAYMENT   PERFORM DO-PAYMENT
        WHEN ICF-STEP      PERFORM DO-STEP
        WHEN ICF-PAYOFF    PERFORM DO-PAYOFF
        WHEN OTHER
            MOVE 8 TO IC-RC
            MOVE "INTCAL: UNKNOWN FUNCTION" TO IC-MSG
    END-EVALUATE
    GOBACK.

      *> truncating decimal division: W-NQ = W-NA / W-NB
NDIV.
    DIVIDE W-NA BY W-NB GIVING W-NQ REMAINDER W-NR.

      *> monthly rate = annual rate / 12, truncated at 10 decimals
MONTHLY-RATE.
    MOVE IC-RATE TO W-NA
    MOVE 12 TO W-NB
    PERFORM NDIV
    MOVE W-NQ TO W-MRATE.

      *> round a high precision figure to whole cents, half away from zero
ROUND-CENTS.
    IF W-TEMP < 0
        COMPUTE W-TEMP2 = W-TEMP - 0.005
    ELSE
        COMPUTE W-TEMP2 = W-TEMP + 0.005
    END-IF
    MOVE W-TEMP2 TO IC-OUT.

      *> ==== simple interest for N days on an actual/365 basis ==========
      *> Operates only on the dedicated W-S* fields so that a caller cannot
      *> confuse the day count with the day-count basis.
DO-SIMPLE.
    MOVE IC-AMT1 TO W-SBAL
    MOVE IC-RATE TO W-SRATE
    MOVE IC-N TO W-SDAYS
    MOVE 365 TO W-SBASIS
    IF IC-AMT2 > 0
        MOVE IC-AMT2 TO W-SBASIS
    END-IF
    PERFORM SIMPLE-CORE.

SIMPLE-CORE.
    COMPUTE W-TEMP = W-SBAL * W-SRATE
    COMPUTE W-TEMP = W-TEMP * W-SDAYS
    MOVE W-TEMP TO W-NA
    MOVE W-SBASIS TO W-NB
    PERFORM NDIV
    MOVE W-NQ TO W-TEMP
    PERFORM ROUND-CENTS.

      *> ==== savings credit, nothing paid below the minimum balance =====
      *> AMT1 balance, AMT2 days, AMT3 minimum balance to earn interest.
DO-SAVINGS.
    MOVE 0 TO IC-OUT
    IF IC-AMT3 > 0 AND IC-AMT1 <= IC-AMT3
        MOVE "BELOW MINIMUM BALANCE - NO CREDIT" TO IC-MSG
        EXIT PARAGRAPH
    END-IF
    MOVE IC-AMT1 TO IC-OUT3
    MOVE IC-AMT1 TO W-SBAL
    MOVE IC-RATE TO W-SRATE
    MOVE IC-AMT2 TO W-SDAYS
    MOVE 365 TO W-SBASIS
    PERFORM SIMPLE-CORE.

      *> ==== monthly payment on an amortising loan ========================
      *> payment = principal / annuity factor, annuity factor = sum of
      *> 1/(1+r)^k for k = 1..n. Summation keeps every step in packed
      *> decimal, which a POWER function would not.
DO-PAYMENT.
    MOVE IC-N TO W-NTERM
    IF W-NTERM < 1
        MOVE 8 TO IC-RC
        MOVE "TERM MUST BE AT LEAST ONE MONTH" TO IC-MSG
        EXIT PARAGRAPH
    END-IF
    IF IC-AMT1 <= 0
        MOVE 0 TO IC-OUT
        EXIT PARAGRAPH
    END-IF
    PERFORM MONTHLY-RATE
    MOVE 1 TO W-DISC
    MOVE 0 TO W-AF
    PERFORM VARYING W-I FROM 1 BY 1 UNTIL W-I > W-NTERM
        COMPUTE W-DISC = W-DISC * (1 + W-MRATE)
        MOVE 1 TO W-NA
        MOVE W-DISC TO W-NB
        PERFORM NDIV
        ADD W-NQ TO W-AF
    END-PERFORM
    IF W-AF = 0
        MOVE 0 TO IC-OUT
        EXIT PARAGRAPH
    END-IF
    MOVE IC-AMT1 TO W-NA
    MOVE W-AF TO W-NB
    PERFORM NDIV
    MOVE W-NQ TO W-TEMP
    PERFORM ROUND-CENTS.

      *> ==== one amortisation step ========================================
      *> IN  principal outstanding, payment, annual rate.
      *> OUT interest, principal reduction, balance after the step.
DO-STEP.
    PERFORM MONTHLY-RATE
    COMPUTE W-TEMP = IC-AMT1 * W-MRATE
    PERFORM ROUND-CENTS
    COMPUTE IC-OUT2 = IC-AMT2 - IC-OUT
    IF IC-OUT2 > IC-AMT1
        MOVE IC-AMT1 TO IC-OUT2
    END-IF
    IF IC-OUT2 < 0
        MOVE 0 TO IC-OUT2
    END-IF
    COMPUTE IC-OUT3 = IC-AMT1 - IC-OUT2.

      *> ==== payoff quote = principal plus accrued interest ===============
DO-PAYOFF.
    MOVE IC-AMT1 TO W-SAVPRIN
    MOVE IC-AMT1 TO W-SBAL
    MOVE IC-RATE TO W-SRATE
    MOVE IC-AMT2 TO W-SDAYS
    MOVE 365 TO W-SBASIS
    PERFORM SIMPLE-CORE
    COMPUTE IC-OUT3 = W-SAVPRIN + IC-OUT.
