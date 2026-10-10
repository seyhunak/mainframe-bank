      *> ====================================================================
      *> BANKCON  -  Global constants, codes and shared edit masks
      *> MERIDIAN TRUST / COBOL CORE BANKING SYSTEM
      *> ====================================================================

       01  BK-MONEY.
           05  BK-AMT      PIC S9(13)V99 COMP-3.     *> transaction amount
           05  BK-BAL      PIC S9(13)V99 COMP-3.     *> ledger balance
           05  BK-LOANAMT  PIC S9(13)V99 COMP-3.     *> loan money
           05  BK-RATE     PIC S9(5)V9(10) COMP-3.   *> annual rate (decimal)

       01  BK-HIGH.
           05  BK-TOT      PIC S9(17)V9(10) COMP-3.  *> scratch for precision
           05  BK-IJ       PIC S9(17)V99 COMP-3.

       01  BK-EDIT.
           05  BK-AMT-ED   PIC -Z(11)9.99.   *> floating, for messages
           05  BK-BAL-C    PIC --,---,---,---,--9.99. *> comma, for columns
           05  BK-RATE-ED  PIC Z9.99999999.
           05  BK-CNT-ED   PIC ZZZ,ZZ9.
           05  BK-YN-ED    PIC Z9.

       01  BK-TIMES.
           05  BK-DATE     PIC 9(8).
           05  BK-TIME     PIC 9(6).
           05  BK-DATEED   PIC 9(8).
           05  BK-SEQNO    PIC 9(12).
           05  BK-TBLSEQ   PIC 9(12).

       01  BK-USER.
           05  BK-USRID    PIC X(8).
           05  BK-USRNM    PIC X(30).
           05  BK-TERMID   PIC X(4).
           05  BK-TELLERID PIC X(6).
           05  BK-BRANCH   PIC 9(4).

      *>> ------------------ ACCOUNT TYPE CODES ----------------------<<
       01  BK-ATYPE.
           05  AT-CHK      PIC X VALUE "C".
           05  AT-SAV      PIC X VALUE "S".
           05  AT-MMA      PIC X VALUE "M".
           05  AT-LOC      PIC X VALUE "L".
       88  AT-CHECKING    VALUE "C".
       88  AT-SAVINGS     VALUE "S".
       88  AT-MONEYMRK    VALUE "M".
       88  AT-LINEOCRC    VALUE "L".

      *>> ------------------ ACCOUNT STATUS --------------------------<<
       01  BK-ASTAT.
           05  AST-ACTV    PIC X VALUE "A".
           05  AST-DORM    PIC X VALUE "D".
           05  AST-CLOS    PIC X VALUE "C".
           05  AST-FROZ    PIC X VALUE "F".
       88  AST-ACTIVE    VALUE "A".
       88  AST-DORMANT   VALUE "D".
       88  AST-CLOSED    VALUE "C".
       88  AST-FROZEN    VALUE "F".

      *>> ------------------ TRANSACTION TYPES ------------------------<<
       01  BK-TTYPE.
           05  TT-DEP      PIC X(2) VALUE "DP".
           05  TT-WDL      PIC X(2) VALUE "WD".
           05  TT-XFR      PIC X(2) VALUE "XF".
           05  TT-XFEE     PIC X(2) VALUE "XF".    *> transfer fee posting
           05  TT-LOANOP   PIC X(2) VALUE "LO".
           05  TT-LOANDD   PIC X(2) VALUE "LD".    *> loan disbursement
           05  TT-LOANPM   PIC X(2) VALUE "LP".    *> loan payment
           05  TT-INTCR    PIC X(2) VALUE "IC".    *> interest credit
           05  TT-FEE      PIC X(2) VALUE "FE".    *> fee assessment
           05  TT-NSF      PIC X(2) VALUE "NS".    *> returned item / NSF
           05  TT-ADJ      PIC X(2) VALUE "AJ".    *> adjustment
           05  TT-CHRG     PIC X(2) VALUE "CH".    *> chargeback / reversal

      *>> ------------------ CHANNEL CODES ---------------------------<<
       01  BK-CHAN.
           05  CH-BRCH     PIC X(2) VALUE "BR".
           05  CH-ATM      PIC X(2) VALUE "AT".
           05  CH-ONLN     PIC X(2) VALUE "ON".
           05  CH-BATCH    PIC X(2) VALUE "BA".
           05  CH-SYS      PIC X(2) VALUE "SY".

      *>> ------------------ RETURN / CONDITION CODES ----------------<<
       01  BK-RCODE       PIC S9(4) COMP VALUE 0.
       01  BK-RC-TBL.
           05  BK-RC-DESC  PIC X(52).
       01  RC-SUCCESS     PIC S9(4) COMP VALUE 0.
       01  RC-BADKEY      PIC S9(4) COMP VALUE 4.
       01  RC-NOFUNDS     PIC S9(4) COMP VALUE 8.
       01  RC-BADAMT      PIC S9(4) COMP VALUE 12.
       01  RC-ACCTST      PIC S9(4) COMP VALUE 16.
       01  RC-LIMEXCD     PIC S9(4) COMP VALUE 20.
       01  RC-BALNEG      PIC S9(4) COMP VALUE 24.
       01  RC-DUPKEY      PIC S9(4) COMP VALUE 28.
       01  RC-INVLOAN     PIC S9(4) COMP VALUE 32.
       01  RC-DELQTE      PIC S9(4) COMP VALUE 36.
       01  RC-IOTERR      PIC S9(4) COMP VALUE 40.
       01  RC-NOFILING    PIC S9(4) COMP VALUE 44.
       01  RC-BADREQ      PIC S9(4) COMP VALUE 48.
       01  RC-LOCKED      PIC S9(4) COMP VALUE 52.
       01  RC-SAMEDAY     PIC S9(4) COMP VALUE 56.

      *>> ------------------ LOAN TYPE / DELINQENCY -----------------<<
       01  BK-LTYPE.
           05  LT-AUTO     PIC X VALUE "A".
           05  LT-MORTG    PIC X VALUE "M".
           05  LT-PERSN    PIC X VALUE "P".
           05  LT-CARD     PIC X VALUE "C".
       01  BK-DLINQ.
           05  DL-CUR      PIC X VALUE "0".
           05  DL-30       PIC X VALUE "1".
           05  DL-60       PIC X VALUE "2".
           05  DL-90       PIC X VALUE "3".
           05  DL-120      PIC X VALUE "4".
           05  DL-WRTOFF   PIC X VALUE "9".
           05  DL-PAID     PIC X VALUE "C".

       *>> ------------------ CHART OF ACCOUNTS ----------------------<<
       *> A real bank holds these in a table; they are constants here so a
       *> posting can name the control accounts it touches.
       01  BK-GL-ACCT.
           05  GA-CASH     PIC X(8) VALUE "1000".
           05  GA-DEP      PIC X(8) VALUE "2000".
           05  GA-LOAN     PIC X(8) VALUE "3000".
           05  GA-INTEXP   PIC X(8) VALUE "4000".
           05  GA-FEEINC   PIC X(8) VALUE "5000".
           05  GA-CTRL     PIC X(8) VALUE "9000".
       01  BK-GL-NAMES.
           05  GAN-CASH    PIC X(30) VALUE "CASH ON HAND".
           05  GAN-DEP     PIC X(30) VALUE "CUSTOMER DEPOSITS".
           05  GAN-LOAN    PIC X(30) VALUE "LOANS RECEIVABLE".
           05  GAN-INTEXP  PIC X(30) VALUE "INTEREST EXPENSE".
           05  GAN-FEEINC  PIC X(30) VALUE "SERVICE FEE INCOME".
           05  GAN-CTRL    PIC X(30) VALUE "LEDGER CONTROL".

      *>> ------------------ OPERATOR AUTHORITY LEVELS --------------<<
       01  BK-TLEVEL.
           05  TL-CLERK         PIC 9 VALUE 1.
           05  TL-TELLER        PIC 9 VALUE 2.
           05  TL-SUPERVISOR    PIC 9 VALUE 3.
           05  TL-MANAGER       PIC 9 VALUE 4.

      *>> ------------------ FILE STATUS ALIASES ---------------------<<
       01  FS-OK          PIC XX VALUE "00".
       01  FS-EOF         PIC XX VALUE "10".
       01  FS-NOTFND      PIC XX VALUE "23".
       01  FS-DUPKEY      PIC XX VALUE "22".

      *>> ------------------ ANSI / TERMINAL ------------------------<<
       01  BK-TERM-CTL.
           05  ANS-CLS     PIC X(10) VALUE X"1B5B326A1B5B48".
           05  ANS-HME     PIC X(10) VALUE X"1B5B48".
           05  ANS-NOR     PIC X(10) VALUE X"1B5B306D".
           05  ANS-REV     PIC X(10) VALUE X"1B5B376D".
           05  ANS-BLD     PIC X(10) VALUE X"1B5B316D".
           05  ANS-UND     PIC X(10) VALUE X"1B5B346D".
           05  ANS-BLK     PIC X(10) VALUE X"1B5B356D".
           05  ANS-CYN     PIC X(10) VALUE X"1B5B33366D".
           05  ANS-YEL     PIC X(10) VALUE X"1B5B33336D".
           05  ANS-CLEAR   PIC X(10) VALUE X"1B5B326D".
           05  ANS-CSI     PIC X(2)  VALUE X"1B5B".
           05  BK-SCRW     PIC 99 VALUE 80.
           05  BK-SCRL     PIC 99 VALUE 24.
