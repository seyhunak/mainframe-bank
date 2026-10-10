      *> ====================================================================
      *> DATECOMM - COMMAREA for the MBDATE date arithmetic service.
      *> NOTE: a commarea passed BY REFERENCE must be ONE 01 level group.
      *> A second 01 level here would sit outside the linkage area that
      *> CALL ... USING actually transmits, so the constants below are
      *> deliberately subordinate to DTECOMM rather than a sibling group.
      *> ====================================================================
       01  DTECOMM.
           05  DT-FUNC          PIC X(4).
           05  DT-IN1           PIC 9(8).
           05  DT-IN2           PIC 9(8).
           05  DT-IN-NUM        PIC S9(9) COMP-3.
           05  DT-OUT           PIC 9(8).
           05  DT-OUT-NUM       PIC S9(9) COMP-3.
           05  DT-RC            PIC S9(4) COMP.
           05  DT-FLG           PIC X(4).
           05  DF-VALID         PIC X(4) VALUE "VALD".
           05  DF-EOM           PIC X(4) VALUE "EOMF".
           05  DF-AMON          PIC X(4) VALUE "AMON".
           05  DF-ADAY          PIC X(4) VALUE "ADAY".
           05  DF-DBTW          PIC X(4) VALUE "DBTW".
           05  DF-DOW           PIC X(4) VALUE "DOW ".
           05  DF-DOM           PIC X(4) VALUE "DOM ".
           05  DF-NXTBUS        PIC X(4) VALUE "NBBU".
           05  DF-MYEOM         PIC X(4) VALUE "MEOM".
           05  DF-DMON          PIC X(4) VALUE "DMON".
           05  DF-YRBW          PIC X(4) VALUE "YRBW".
