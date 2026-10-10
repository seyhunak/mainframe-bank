>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBSCRN - Terminal screen manager, in the style of a CICS map.
       *>
       *> The caller composes a 24 by 80 screen buffer inside a SCRCOMM
       *> commarea and asks for it to be shown. The cursor is left wherever
       *> the caller asked, so a prompt followed by an ACCEPT behaves like a
       *> protected field followed by an unprotected one.
       *>
       *> The caller also carries the session context (operator, terminal,
       *> date, time, authority) in SCRCOMM. Nothing is taken from this
       *> module's own storage: a called program's copy of a common area is
       *> its own, and the header would otherwise be permanently blank.
       *>
       *> COMMAREA dispatch, the action carried in SC-ACTION:
       *>   CLS   blank the screen buffer
       *>   HDR   standard bank header on rows 1 and 2
       *>   LINE  write SC-IN at SC-ROW in the attribute named by SC-ATTR
       *>   RULE  draw a horizontal rule at SC-ROW
       *>   FTR   write SC-MSG on the message row, highlighted
       *>   SHOW  paint the buffer and park the cursor at SC-ROW / SC-COL
       *>   FLD   print SC-MSG as a prompt and ACCEPT the reply into SC-IN
       *>   NUM   as FLD, but the reply must parse as money
       *>   YN    as FLD, but the reply must be Y or N
       *>
       *> Attributes for LINE: N plain, R reverse, B bold, U underline,
       *> Y yellow, C cyan. Styled rows run to column 76; the attribute is
       *> reset in 77-80 so it cannot bleed into the next line.
       *> ====================================================================
IDENTIFICATION DIVISION.
PROGRAM-ID. MBSCRN.

DATA DIVISION.
WORKING-STORAGE SECTION.
      *> BANKCON is copied for the terminal constants only: the ANSI
      *> attributes and the 24x80 geometry. Its session fields (BK-USRID,
      *> BK-DATE, ...) are never read here - they belong to the caller, and
      *> reading this module's own copy is what left the header blank.
COPY BANKCON.
COPY MONEYCOMM.

01  W-R         PIC 99 COMP VALUE 0.
01  W-ROWED     PIC 99.
01  W-COLED     PIC 99.
01  W-CURSEQ    PIC X(12).
01  W-HAVEANS   PIC 9 COMP VALUE 0.

LINKAGE SECTION.
COPY SCRCOMM.

PROCEDURE DIVISION USING SCRCOMM.

MAIN-PARA.
    EVALUATE SC-ACTION
        WHEN SCA-CLS      PERFORM DO-CLEAR
        WHEN SCA-HDR      PERFORM DO-HEADER
        WHEN SCA-LINE     PERFORM DO-LINE
        WHEN SCA-RULE     PERFORM DO-RULE
        WHEN SCA-FTR      PERFORM DO-FOOTER
        WHEN SCA-SHOW     PERFORM DO-SHOW
        WHEN SCA-FLD      PERFORM DO-FIELD
        WHEN SCA-NUM      PERFORM DO-NUMBER
        WHEN SCA-YN       PERFORM DO-YESNO
        WHEN OTHER
            DISPLAY "SCRN: UNKNOWN ACTION [" SC-ACTION "]"
    END-EVALUATE
    GOBACK.

DO-CLEAR.
    PERFORM VARYING W-R FROM 1 BY 1 UNTIL W-R > BK-SCRL
        MOVE SPACES TO SC-LINES(W-R)
    END-PERFORM
    MOVE 1 TO SC-ROW
    MOVE 1 TO SC-COL.

DO-HEADER.
    MOVE SPACES TO SC-LINES(1)
    MOVE SPACES TO SC-LINES(2)
    MOVE "MERIDIAN TRUST COMPANY" TO SC-LINES(1)(1:22)
    MOVE "ONLINE BANKING SYSTEM" TO SC-LINES(1)(24:22)
    MOVE "TERMINAL " TO SC-LINES(1)(48:9)
    MOVE SC-TERMID TO SC-LINES(1)(58:4)
    MOVE SPACES TO SC-LINES(1)(61:19)
    MOVE "USER " TO SC-LINES(2)(1:5)
    MOVE SC-USRID TO SC-LINES(2)(7:8)
    MOVE "DATE " TO SC-LINES(2)(20:5)
    MOVE SC-DATE TO SC-LINES(2)(26:8)
    MOVE "TIME " TO SC-LINES(2)(38:5)
    MOVE SC-TIME TO SC-LINES(2)(44:6)
    MOVE "AUTH " TO SC-LINES(2)(56:5)
    MOVE SC-LIMITED TO SC-LINES(2)(62:1).

DO-LINE.
    PERFORM CLAMP-ROW
    IF SC-IN = SPACES
        MOVE SPACES TO SC-LINES(SC-ROW)
    ELSE
        EVALUATE SC-ATTR
            WHEN "R"
                MOVE ANS-REV TO SC-LINES(SC-ROW)(1:4)
                MOVE SC-IN TO SC-LINES(SC-ROW)(5:72)
                MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4)
            WHEN "B"
                MOVE ANS-BLD TO SC-LINES(SC-ROW)(1:4)
                MOVE SC-IN TO SC-LINES(SC-ROW)(5:72)
                MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4)
            WHEN "U"
                MOVE ANS-UND TO SC-LINES(SC-ROW)(1:4)
                MOVE SC-IN TO SC-LINES(SC-ROW)(5:72)
                MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4)
            WHEN "Y"
                MOVE ANS-YEL TO SC-LINES(SC-ROW)(1:5)
                MOVE SC-IN TO SC-LINES(SC-ROW)(6:71)
                MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4)
            WHEN "C"
                MOVE ANS-CYN TO SC-LINES(SC-ROW)(1:5)
                MOVE SC-IN TO SC-LINES(SC-ROW)(6:71)
                MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4)
            WHEN OTHER
                MOVE SC-IN TO SC-LINES(SC-ROW)
        END-EVALUATE
    END-IF.

DO-RULE.
    PERFORM CLAMP-ROW
    MOVE ALL "-" TO SC-LINES(SC-ROW).

DO-FOOTER.
    MOVE BK-SCRL TO SC-ROW.
    PERFORM CLAMP-ROW.
    MOVE SPACES TO SC-LINES(SC-ROW).
    MOVE ANS-BLK TO SC-LINES(SC-ROW)(1:4).
    MOVE SC-MSG TO SC-LINES(SC-ROW)(5:72).
    MOVE ANS-NOR TO SC-LINES(SC-ROW)(77:4).

CLAMP-ROW.
    IF SC-ROW < 1
        MOVE 1 TO SC-ROW
    END-IF
    IF SC-ROW > BK-SCRL
        MOVE BK-SCRL TO SC-ROW
    END-IF.

      *> ---- paint the buffer, then park the cursor ----------------------
DO-SHOW.
    DISPLAY ANS-CLS WITH NO ADVANCING
    PERFORM VARYING W-R FROM 1 BY 1 UNTIL W-R > BK-SCRL
        DISPLAY SC-LINES(W-R)
    END-PERFORM
    PERFORM POSITION-CURSOR.

POSITION-CURSOR.
    IF SC-ROW < 1
        MOVE 1 TO SC-ROW
    END-IF
    IF SC-ROW > BK-SCRL
        MOVE BK-SCRL TO SC-ROW
    END-IF
    IF SC-COL < 1
        MOVE 1 TO SC-COL
    END-IF
    IF SC-COL > BK-SCRW
        MOVE BK-SCRW TO SC-COL
    END-IF
    MOVE SPACES TO W-CURSEQ
    MOVE ANS-CSI TO W-CURSEQ(1:2)
    MOVE SC-ROW TO W-ROWED
    MOVE SC-COL TO W-COLED
    MOVE W-ROWED TO W-CURSEQ(3:2)
    MOVE ";" TO W-CURSEQ(5:1)
    MOVE W-COLED TO W-CURSEQ(6:2)
    MOVE "H" TO W-CURSEQ(8:1)
    DISPLAY W-CURSEQ WITH NO ADVANCING.

      *> ---- prompt and read one line into SC-IN -------------------------
DO-FIELD.
    DISPLAY SC-MSG WITH NO ADVANCING
    ACCEPT SC-IN FROM CONSOLE.

      *> ---- prompt and read, requiring a valid money field ---------------
      *> A malformed amount is reprompted rather than posted, which is the
      *> whole reason the parser exists. MN-RC starts out non-zero: a
      *> PERFORM UNTIL tests before it runs, so seeding it with zero made
      *> this paragraph unreachable and every amount silently became 0.00.
DO-NUMBER.
    MOVE 12 TO MN-RC.
    PERFORM UNTIL MN-RC = 0
        MOVE SPACES TO MN-IN
        DISPLAY SC-MSG WITH NO ADVANCING
        ACCEPT SC-IN FROM CONSOLE
        MOVE SC-IN TO MN-IN
        MOVE MN-FN-VALID TO MN-FUNC
        CALL "MBMONEY" USING MONEYCOMM
        IF MN-RC NOT = 0
            DISPLAY MN-MSG
        END-IF
    END-PERFORM
    MOVE MN-OUT TO SC-NUM
    MOVE MN-OUT-ED TO SC-OUT.

      *> ---- prompt and read, requiring Y or N ----------------------------
DO-YESNO.
    MOVE 0 TO W-HAVEANS
    PERFORM UNTIL W-HAVEANS = 1
        DISPLAY SC-MSG WITH NO ADVANCING
        ACCEPT SC-IN FROM CONSOLE
        EVALUATE FUNCTION UPPER-CASE(FUNCTION TRIM(SC-IN))
            WHEN "Y"
                MOVE 1 TO W-HAVEANS
            WHEN "N"
                MOVE 1 TO W-HAVEANS
            WHEN OTHER
                MOVE "ANSWER Y OR N" TO SC-MSG
        END-EVALUATE
    END-PERFORM.
