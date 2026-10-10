      *> ====================================================================
      *> INTCOMM - COMMAREA for MBINTCAL, the interest and amortisation engine.
      *> Rates are annual decimals held as S9(5)V9(10) so that basis points and
      *> promotional fractions survive a round trip without rounding.
      *> ====================================================================
       01  INTCOMM.
           05  IC-FUNC        PIC X(4).
           05  IC-AMT1        PIC S9(13)V99 COMP-3.
           05  IC-AMT2        PIC S9(13)V99 COMP-3.
           05  IC-AMT3        PIC S9(13)V99 COMP-3.
           05  IC-RATE        PIC S9(5)V9(10) COMP-3.
           05  IC-N           PIC S9(9) COMP-3.
           05  IC-N2          PIC S9(9) COMP-3.
           05  IC-OUT         PIC S9(13)V99 COMP-3.
           05  IC-OUT2        PIC S9(13)V99 COMP-3.
           05  IC-OUT3        PIC S9(13)V99 COMP-3.
           05  IC-RC          PIC S9(4) COMP.
           05  IC-MSG         PIC X(60).
           05  ICF-SIMPLE     PIC X(4) VALUE "SIMP".
           05  ICF-SAVTIER    PIC X(4) VALUE "SAVT".
           05  ICF-PAYMENT    PIC X(4) VALUE "AMOR".
           05  ICF-STEP       PIC X(4) VALUE "STEP".
           05  ICF-PAYOFF     PIC X(4) VALUE "PAYF".
           05  ICF-TOTALINT   PIC X(4) VALUE "TINT".
