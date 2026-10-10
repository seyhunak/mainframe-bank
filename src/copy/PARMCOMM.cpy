      *> ====================================================================
      *> PARMCOMM - COMMAREA for MBPARM, the parameter service.     DSORG=Indexed KSDS
      *> Every fee, rate and limit a real bank holds in a table rather than
      *> in program literals, so a rate change is a data change.
      *> ====================================================================
       01  PARMCOMM.
           05  PM-NAME        PIC X(12).
           05  PM-VALUE       PIC S9(13)V99 COMP-3.
           05  PM-TEXT        PIC X(30).
           05  PM-RC          PIC S9(4) COMP.
           05  PM-MSG         PIC X(40).
           05  PM-FUNC        PIC X(4).
           05  PM-FN-GET      PIC X(4) VALUE "GET ".
           05  PM-FN-SET      PIC X(4) VALUE "SET ".
           05  PM-FN-NEXT     PIC X(4) VALUE "NEXT".

