      *> ====================================================================
      *> LOGCOMM - COMMAREA for MBLOGN, the sign-on service.
      *> In goes the operator id and the password typed at the terminal;
      *> out comes the teller record, or a reason code.
      *> ====================================================================
       01  LOGCOMM.
           05  LG-ID           PIC X(8).
           05  LG-PASSWD       PIC X(8).
           05  LG-NAME         PIC X(30).
           05  LG-BRANCH       PIC 9(4).
           05  LG-LEVEL        PIC 9.
           05  LG-LIMIT        PIC S9(13)V99 COMP-3.
           05  LG-CASH-LIMIT   PIC S9(13)V99 COMP-3.
           05  LG-RC           PIC S9(4) COMP.
           05  LG-MSG          PIC X(40).
           05  LG-FUNC         PIC X(4).
           05  LG-FN-ON        PIC X(4) VALUE "ON  ".
           05  LG-FN-OFF       PIC X(4) VALUE "OFF ".
