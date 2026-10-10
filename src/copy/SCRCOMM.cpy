      *> ====================================================================
      *> SCRCOMM - Terminal COMMAREA shared by MBSCRN and its callers.
      *> Mirrors a CICS COMMAREA: caller fills the request area, the screen
      *> manager leaves the reply in the same structure. Passed BY REFERENCE
      *> so screen state round-trips without copying.
      *> SC-USRID and friends are here because a called module must never
      *> carry session context in its own storage: that copy is its own.
      *> ====================================================================
       01  SCRCOMM.
           05  SC-ACTION        PIC X(4).
           05  SC-ROW           PIC 99 COMP-3.
           05  SC-COL           PIC 99 COMP-3.
           05  SC-ATTR          PIC X(1).
           05  SC-NUM           PIC S9(13)V99 COMP-3.
           05  SC-NUM2          PIC S9(13)V99 COMP-3.
           05  SC-CNT           PIC 9(9) COMP-3.
           05  SC-IN            PIC X(80).
           05  SC-OUT           PIC X(80).
           05  SC-MSG           PIC X(80).
           05  FILLER           PIC X(8).
           05  SC-LINES OCCURS 24 TIMES PIC X(80).
           05  SC-USRID         PIC X(8).
           05  SC-USRNM         PIC X(30).
           05  SC-TERMID        PIC X(4).
           05  SC-TELLERID      PIC X(6).
           05  SC-BRANCH        PIC 9(4).
           05  SC-LEVEL         PIC 9.
           05  SC-LIMITED       PIC X(1).
           05  SC-DATE          PIC 9(8).
           05  SC-TIME          PIC 9(6).
           05  SC-LIMIT         PIC S9(13)V99 COMP-3.
           05  SCA-CLS          PIC X(4) VALUE "CLS ".
           05  SCA-HDR          PIC X(4) VALUE "HDR ".
           05  SCA-LINE         PIC X(4) VALUE "LINE".
           05  SCA-FTR          PIC X(4) VALUE "FTR ".
           05  SCA-RULE         PIC X(4) VALUE "RULE".
           05  SCA-SHOW         PIC X(4) VALUE "SHOW".
           05  SCA-FLD          PIC X(4) VALUE "FLD ".
           05  SCA-MENU         PIC X(4) VALUE "MENU".
           05  SCA-NUM          PIC X(4) VALUE "NUM ".
           05  SCA-YN           PIC X(4) VALUE "YN  ".
           05  SCA-CLEAR        PIC X(4) VALUE "BLK ".
