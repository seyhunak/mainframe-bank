      *> ====================================================================
      *> GLCOMM - COMMAREA for MBGL, the control-account service.
      *> One side of one ledger line: account, side (D/C) and amount.
      *> Named GC-* so a program that holds both the ledger entry record
      *> and this commarea never has to qualify a field.
      *> ====================================================================
       01  GLCOMM.
           05  GC-ACCT         PIC X(8).
           05  GC-NAME         PIC X(30).
           05  GC-SIDE         PIC X(1).
           05  GC-AMOUNT       PIC S9(13)V99 COMP-3.
           05  GC-RC           PIC S9(4) COMP.
           05  GC-MSG          PIC X(40).
           05  GC-FUNC         PIC X(4).
           05  GC-FN-POST      PIC X(4) VALUE "POST".
