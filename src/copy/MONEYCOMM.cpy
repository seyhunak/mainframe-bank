      *> ====================================================================
      *> MONEYCOMM - COMMAREA for MBMONEY, the money field service.
      *> Amounts are S9(13)V99 COMP-3 (packed decimal) everywhere inside the
      *> system. Text crosses the terminal boundary as DISPLAY and is validated
      *> here before it is allowed to become a ledger figure.
      *> ====================================================================
       01  MONEYCOMM.
           05  MN-FUNC         PIC X(4).
           05  MN-IN           PIC X(24).
           05  MN-OUT          PIC S9(13)V99 COMP-3.
           05  MN-OUT-ED       PIC -Z(11)9.99.
           05  MN-A            PIC S9(13)V99 COMP-3.
           05  MN-B            PIC S9(13)V99 COMP-3.
           05  MN-C            PIC S9(13)V99 COMP-3.
           05  MN-RC           PIC S9(4) COMP.
           05  MN-MSG          PIC X(60).
           05  MN-FN-VALID     PIC X(4) VALUE "PARS".
           05  MN-FN-FMT       PIC X(4) VALUE "FMT ".
           05  MN-FN-NEG       PIC X(4) VALUE "NEG ".
           05  MN-FN-ABS       PIC X(4) VALUE "ABS ".
           05  MN-FN-ADD       PIC X(4) VALUE "ADD ".
           05  MN-FN-SUB       PIC X(4) VALUE "SUB ".
           05  MN-FN-MUL       PIC X(4) VALUE "MUL ".
           05  MN-FN-CMP       PIC X(4) VALUE "CMP ".
           05  MN-FN-CENTS     PIC X(4) VALUE "RND ".
