>>SOURCE FORMAT FREE
       *> ====================================================================
       *> MBLD001 - Dataset bootstrap.
       *>
       *> Creates every dataset from scratch and loads a book of customers,
       *> accounts and loans so the terminal and the batch jobs have real
       *> work to do. Re-running wipes the datasets, exactly as a system
       *> generation or a re-run of the production load job would.
       *>
       *> Keying scheme:   customer  Cnnnnnnnnn
       *>                  checking  1nnnn00000
       *>                  savings   2nnnn00000
       *>                  loan      Lnnnnnnnnnnn
       *>                  teller    Tnnnnnnn
       *> so the numbers a teller types on screen are easy to read aloud.
       *>
       *> Every date is produced through MBDATE. The earlier version of this
       *> program added days to a YYYYMMDD field arithmetically, which
       *> produced dates like 20190176 - day seventy-six of January.
       *>
       *> Run:  bin/LOADLIB
       *> ====================================================================
       IDENTIFICATION DIVISION.
       PROGRAM-ID. MBLD001.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT CUST-FILE ASSIGN TO "data/CUSTFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS CU-ID
               FILE STATUS IS FS-CUST.
           SELECT ACCT-FILE ASSIGN TO "data/ACCTFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS AC-NO
               FILE STATUS IS FS-ACCT.
           SELECT BAL-FILE ASSIGN TO "data/BALFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS BA-NO
               FILE STATUS IS FS-BAL.
           SELECT LOAN-FILE ASSIGN TO "data/LOANFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LN-NO
               FILE STATUS IS FS-LOAN.
           SELECT LNBAL-FILE ASSIGN TO "data/LNBALFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LB-NO
               FILE STATUS IS FS-LNBAL.
           SELECT TXN-FILE ASSIGN TO "data/TXNFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TX-SEQ
               FILE STATUS IS FS-TXN.
           SELECT AUD-FILE ASSIGN TO "data/AUDFILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-AUD.
           SELECT SEQ-FILE ASSIGN TO "data/SEQFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS SQ-NAME
               FILE STATUS IS FS-SEQ.
           SELECT TELL-FILE ASSIGN TO "data/TELLFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS TE-ID
               FILE STATUS IS FS-TELL.
           SELECT PARM-FILE ASSIGN TO "data/PARMFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS PRM-NAME
               FILE STATUS IS FS-PARM.
           SELECT LOCK-FILE ASSIGN TO "data/LOCKFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS LK-KEY
               FILE STATUS IS FS-LOCK.
           SELECT GL-FILE ASSIGN TO "data/GLFILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-GL.
           SELECT GLB-FILE ASSIGN TO "data/GLBFILE"
               ORGANIZATION IS INDEXED
               ACCESS MODE IS RANDOM
               RECORD KEY IS GB-ACCT
               FILE STATUS IS FS-GLB.
           SELECT STMT-FILE ASSIGN TO "data/STMTFILE"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-STMT.
           SELECT JOBLOG-FILE ASSIGN TO "data/JOBLOG"
               ORGANIZATION IS SEQUENTIAL
               FILE STATUS IS FS-JOB.

       DATA DIVISION.
       FILE SECTION.
       FD  CUST-FILE.
       COPY CUSTREC.
       FD  ACCT-FILE.
       COPY ACCTREC.
       FD  BAL-FILE.
       COPY BALREC.
       FD  LOAN-FILE.
       COPY LOANREC.
       FD  LNBAL-FILE.
       COPY LNBALREC.
       FD  TXN-FILE.
       COPY TXNREC.
       FD  AUD-FILE.
       COPY AUDREC.
       FD  SEQ-FILE.
       COPY SEQREC.
       FD  TELL-FILE.
       COPY TELLREC.
       FD  PARM-FILE.
       COPY PARMREC.
       FD  LOCK-FILE.
       COPY LOCKREC.
       FD  GL-FILE.
       COPY GLREC.
       FD  GLB-FILE.
       COPY GLBREC.
       FD  STMT-FILE.
       COPY STMTREC.
       FD  JOBLOG-FILE.
       COPY JOBREC.

       WORKING-STORAGE SECTION.
       COPY BANKCON.
       COPY DATECOMM.

       01  FS-CUST     PIC XX.
       01  FS-ACCT     PIC XX.
       01  FS-BAL      PIC XX.
       01  FS-LOAN     PIC XX.
       01  FS-LNBAL    PIC XX.
       01  FS-TXN      PIC XX.
       01  FS-AUD      PIC XX.
       01  FS-SEQ      PIC XX.
       01  FS-TELL     PIC XX.
       01  FS-PARM     PIC XX.
       01  FS-LOCK     PIC XX.
       01  FS-GL       PIC XX.
       01  FS-GLB      PIC XX.
       01  FS-STMT     PIC XX.
       01  FS-JOB      PIC XX.

       01  W-CUST-N    PIC 9(4) VALUE 60.
       01  W-ACCT-N    PIC 9(4) VALUE 120.

       01  W-SURNAME-TBL.
           05  W-SN OCCURS 20 TIMES PIC X(14).
       01  W-GIVEN-TBL.
           05  W-GN OCCURS 12 TIMES PIC X(12).
       01  W-CITY-TBL.
           05  W-CT OCCURS 8 TIMES PIC X(14).

       01  W-N         PIC 9(4) COMP-3 VALUE 0.
       01  W-N4        PIC 9(4).
       01  W-N9        PIC 9(9).
       01  W-CUSTID    PIC X(10).
       01  W-NAME      PIC X(30).
       01  W-BRANCH    PIC 9(4).
       01  W-OPEN-DT   PIC 9(8).
       01  W-DOB       PIC 9(8).
       01  W-ZIP9      PIC 9(9).
       01  W-AMT       PIC S9(13)V99 COMP-3.
       01  W-ACCT-TYPE PIC X.
       01  W-TOT-DEP   PIC S9(15)V99 COMP-3 VALUE 0.
       01  W-OPEN-CHK  PIC S9(13)V99 COMP-3.
       01  W-OPEN-SAV  PIC S9(13)V99 COMP-3.
       01  W-ROW       PIC 99 COMP VALUE 0.

       PROCEDURE DIVISION.
       MAIN-PARA.
           DISPLAY " "
           DISPLAY "==========================================================="
           DISPLAY "  MBLD001   DATASET BOOTSTRAP"
           DISPLAY "==========================================================="
           PERFORM LOAD-TABLES
           PERFORM CREATE-DATASETS
           PERFORM LOAD-CUSTOMERS
           PERFORM LOAD-LOANS
           PERFORM LOAD-TELLERS
           PERFORM LOAD-PARAMETERS
           PERFORM INIT-COUNT
           PERFORM OPEN-LEDGER
           DISPLAY " "
           DISPLAY "   60 customers, 120 accounts, 4 loans written to ./data"
           DISPLAY " "
           CLOSE CUST-FILE ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE
           CLOSE TXN-FILE SEQ-FILE TELL-FILE PARM-FILE LOCK-FILE
           CLOSE GL-FILE GLB-FILE STMT-FILE JOBLOG-FILE
           GOBACK.

       LOAD-TABLES.
           MOVE "ALDERMAN"    TO W-SN(1)
           MOVE "OKONKWO"     TO W-SN(2)
           MOVE "VASQUEZ"     TO W-SN(3)
           MOVE "THORNBURY"   TO W-SN(4)
           MOVE "NAKASHIMA"   TO W-SN(5)
           MOVE "O'CONNELL"   TO W-SN(6)
           MOVE "MBEKI"       TO W-SN(7)
           MOVE "LINDQVIST"   TO W-SN(8)
           MOVE "FERREIRA"    TO W-SN(9)
           MOVE "WHITLOCK"    TO W-SN(10)
           MOVE "ABARA"       TO W-SN(11)
           MOVE "DUBOIS"      TO W-SN(12)
           MOVE "HALVORSEN"   TO W-SN(13)
           MOVE "MARCHETTI"   TO W-SN(14)
           MOVE "OYELARAN"    TO W-SN(15)
           MOVE "TREMBLAY"    TO W-SN(16)
           MOVE "KOWALSKI"    TO W-SN(17)
           MOVE "NAVARRO"     TO W-SN(18)
           MOVE "SUTHERLAND"  TO W-SN(19)
           MOVE "CHANDRAN"    TO W-SN(20)
           MOVE "MARGARET R"  TO W-GN(1)
           MOVE "DANIEL A"    TO W-GN(2)
           MOVE "LINNETTE"    TO W-GN(3)
           MOVE "EDWARD J"    TO W-GN(4)
           MOVE "HIROSHI"     TO W-GN(5)
           MOVE "SIOBHAN R"   TO W-GN(6)
           MOVE "THANDIWE"    TO W-GN(7)
           MOVE "ANDERS"      TO W-GN(8)
           MOVE "BEATRIZ"     TO W-GN(9)
           MOVE "SAMUEL T"    TO W-GN(10)
           MOVE "CHINEDU"     TO W-GN(11)
           MOVE "MARGUERITE"  TO W-GN(12)
           MOVE "PORTLAND"    TO W-CT(1)
           MOVE "GRESHAM"     TO W-CT(2)
           MOVE "BEAVERTON"   TO W-CT(3)
           MOVE "TIGARD"      TO W-CT(4)
           MOVE "HILLSBORO"   TO W-CT(5)
           MOVE "LAKE OSWEGO" TO W-CT(6)
           MOVE "SALEM"       TO W-CT(7)
           MOVE "GRESHAM"     TO W-CT(8).

           *> ---- create every dataset empty, truncating anything there -------
       CREATE-DATASETS.
           OPEN OUTPUT CUST-FILE ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE
               TXN-FILE AUD-FILE SEQ-FILE TELL-FILE PARM-FILE LOCK-FILE
               GL-FILE GLB-FILE STMT-FILE JOBLOG-FILE
           CLOSE CUST-FILE ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE
           CLOSE TXN-FILE AUD-FILE SEQ-FILE TELL-FILE PARM-FILE LOCK-FILE
           CLOSE GL-FILE GLB-FILE STMT-FILE JOBLOG-FILE
           IF FS-ACCT NOT = "00"
               DISPLAY "   WARNING cannot create account dataset, FS=" FS-ACCT
           END-IF
           IF FS-TELL NOT = "00"
               DISPLAY "   WARNING cannot create teller master, FS=" FS-TELL
           END-IF
           IF FS-GLB NOT = "00"
               DISPLAY "   WARNING cannot create ledger balances, FS=" FS-GLB
           END-IF
             *> the create step truncated and closed everything, so reopen
             *> for update before the load writes any records
           OPEN I-O CUST-FILE ACCT-FILE BAL-FILE LOAN-FILE LNBAL-FILE
               TXN-FILE SEQ-FILE TELL-FILE PARM-FILE LOCK-FILE
               GL-FILE GLB-FILE
           OPEN EXTEND AUD-FILE STMT-FILE JOBLOG-FILE.

       LOAD-CUSTOMERS.
           PERFORM VARYING W-N FROM 1 BY 1 UNTIL W-N > W-CUST-N
               PERFORM LOAD-ONE-CUSTOMER
           END-PERFORM.

       LOAD-ONE-CUSTOMER.
           MOVE W-N TO W-N9
           MOVE W-N9 TO W-N4
             *> customer key, shared by the master and both account records
           MOVE "C" TO W-CUSTID(1:1)
           MOVE W-N9 TO W-CUSTID(2:9)
           MOVE SPACES TO CUST-REC
           MOVE W-CUSTID TO CU-ID
             *> every date goes through MBDATE, never through arithmetic on
             *> the YYYYMMDD field itself
           MOVE 20190115 TO DT-IN1
           COMPUTE DT-IN-NUM = W-N * 37
           MOVE DF-ADAY TO DT-FUNC
           CALL "MBDATE" USING DTECOMM
           MOVE DT-OUT TO CU-OPEN-DT
           MOVE CU-OPEN-DT TO W-OPEN-DT
           MOVE CU-OPEN-DT TO CU-FIRST-DT
           MOVE 0 TO CU-CLOSE-DT
           MOVE 19500210 TO DT-IN1
           COMPUTE DT-IN-NUM = W-N * 173
           MOVE DF-ADAY TO DT-FUNC
           CALL "MBDATE" USING DTECOMM
           MOVE DT-OUT TO CU-DOB
           MOVE CU-DOB TO W-DOB
             *> name: the first twelve books keep the names they always had
           IF W-N <= 12
               MOVE W-SN(W-N) TO W-NAME
               STRING W-NAME DELIMITED BY "  " ", " DELIMITED BY SIZE
                      W-GN(W-N) DELIMITED BY "  " INTO W-NAME
           ELSE
               MOVE W-SN(W-N - 12) TO W-NAME
               STRING W-NAME DELIMITED BY "  " ", " DELIMITED BY SIZE
                      W-GN(W-N - 12) DELIMITED BY "  " INTO W-NAME
           END-IF
           MOVE W-NAME TO CU-NAME
           MOVE "IDENTITY ON FILE - DO NOT MAIL" TO CU-NMEMO
           MOVE "NO FILE IMAGE - IDENTITY ON FILE" TO CU-NMEMO
           IF W-N <= 12
               MOVE W-CT(W-N) TO CU-CITY
           ELSE
               MOVE W-CT(W-N - 12) TO CU-CITY
           END-IF
           MOVE "OR" TO CU-STATE
           COMPUTE W-ZIP9 = 97001 + (W-N * 47)
           MOVE W-ZIP9 TO CU-ZIP(1:9)
           MOVE "503-555-" TO CU-PHONE(1:8)
           MOVE W-N TO W-ZIP9
           COMPUTE W-ZIP9 = 1000 + (W-N * 11)
           MOVE W-ZIP9 TO CU-PHONE(9:4)
           MOVE "A" TO CU-STATUS
           MOVE "A" TO CU-RISK
           COMPUTE W-BRANCH = 1000 + (W-N * 17)
           MOVE W-BRANCH TO CU-BRANCH
           WRITE CUST-REC
             *> ---- checking ---------------------------------------------------
           MOVE "1" TO AC-NO(1:1)
           MOVE "C" TO W-ACCT-TYPE
           IF W-N <= 12
               EVALUATE W-N
                   WHEN 1 MOVE 4820.15 TO W-OPEN-CHK
                   WHEN 2 MOVE 1275.00 TO W-OPEN-CHK
                   WHEN 3 MOVE 25000.00 TO W-OPEN-CHK
                   WHEN 4 MOVE 315.42 TO W-OPEN-CHK
                   WHEN 5 MOVE 18750.75 TO W-OPEN-CHK
                   WHEN 6 MOVE 940.10 TO W-OPEN-CHK
                   WHEN 7 MOVE 31200.00 TO W-OPEN-CHK
                   WHEN 8 MOVE 75.99 TO W-OPEN-CHK
                   WHEN 9 MOVE 6450.00 TO W-OPEN-CHK
                   WHEN 10 MOVE 12980.33 TO W-OPEN-CHK
                   WHEN 11 MOVE 205.60 TO W-OPEN-CHK
                   WHEN OTHER MOVE 5510.90 TO W-OPEN-CHK
               END-EVALUATE
           ELSE
               COMPUTE W-OPEN-CHK = 137.50 + (W-N * 233.75)
           END-IF
           PERFORM LOAD-ACCOUNT
           MOVE W-OPEN-CHK TO W-AMT
           PERFORM WRITE-ONE-ACCOUNT
             *> ---- savings ----------------------------------------------------
           MOVE "2" TO AC-NO(1:1)
           MOVE "S" TO W-ACCT-TYPE
           IF W-N <= 12
               EVALUATE W-N
                   WHEN 1 MOVE 15400.00 TO W-OPEN-SAV
                   WHEN 2 MOVE 320.75 TO W-OPEN-SAV
                   WHEN 3 MOVE 87500.00 TO W-OPEN-SAV
                   WHEN 4 MOVE 6100.50 TO W-OPEN-SAV
                   WHEN 5 MOVE 42300.25 TO W-OPEN-SAV
                   WHEN 6 MOVE 9000.00 TO W-OPEN-SAV
                   WHEN 7 MOVE 12000.00 TO W-OPEN-SAV
                   WHEN 8 MOVE 250.00 TO W-OPEN-SAV
                   WHEN 9 MOVE 38000.00 TO W-OPEN-SAV
                   WHEN 10 MOVE 22550.00 TO W-OPEN-SAV
                   WHEN 11 MOVE 1000.00 TO W-OPEN-SAV
                   WHEN OTHER MOVE 55000.00 TO W-OPEN-SAV
               END-EVALUATE
           ELSE
               COMPUTE W-OPEN-SAV = 611.25 + (W-N * 1200.50)
           END-IF
           PERFORM LOAD-ACCOUNT
           MOVE W-OPEN-SAV TO W-AMT
           PERFORM WRITE-ONE-ACCOUNT.

       LOAD-ACCOUNT.
           MOVE SPACES TO ACCT-REC
           IF W-ACCT-TYPE = "S"
               MOVE "2" TO AC-NO(1:1)
           ELSE
               MOVE "1" TO AC-NO(1:1)
           END-IF
           MOVE W-N4 TO AC-NO(2:4)
           MOVE "00000" TO AC-NO(6:5)
           MOVE W-CUSTID TO AC-CUST-ID
           MOVE "A" TO AC-STATUS
           MOVE "P PERSONAL CHECKING" TO AC-TITLE
           IF W-ACCT-TYPE = "S"
               MOVE "PASSBOOK SAVINGS" TO AC-TITLE
           END-IF
           MOVE W-OPEN-DT TO AC-OPEN-DT
           MOVE 0 TO AC-CLOSE-DT
           MOVE W-BRANCH TO AC-BRANCH
           IF W-ACCT-TYPE = "S"
               MOVE 500 TO AC-MIN-BAL
               MOVE 0 TO AC-OD-LIMIT
               MOVE 0.0425 TO AC-INT-RATE
               MOVE "S" TO AC-TYPE
           ELSE
               MOVE 25 TO AC-MIN-BAL
               MOVE 100 TO AC-OD-LIMIT
               MOVE 0.005 TO AC-INT-RATE
               MOVE "C" TO AC-TYPE
           END-IF
           MOVE 0 TO AC-LTD-CRT
           MOVE 0 TO AC-LTD-DBT
           MOVE 0 TO AC-PREV-BAL.

       WRITE-ONE-ACCOUNT.
           MOVE 0 TO AC-LTD-CRT
           IF W-ACCT-TYPE = "S"
               MOVE W-OPEN-SAV TO AC-LTD-CRT
           ELSE
               MOVE W-OPEN-CHK TO AC-LTD-CRT
           END-IF
           MOVE AC-LTD-CRT TO AC-PREV-BAL
           WRITE ACCT-REC
           IF FS-ACCT NOT = "00"
               DISPLAY "   WARNING account write FS=" FS-ACCT " key=" AC-NO
           END-IF
           MOVE SPACES TO BAL-REC
           MOVE AC-NO TO BA-NO
           MOVE AC-LTD-CRT TO BA-AVAIL
           MOVE AC-LTD-CRT TO BA-CURRENT
           MOVE 0 TO BA-HOLD
           MOVE 0 TO BA-OVERDRAFT
           MOVE 0 TO BA-INT-ACCRUED
           MOVE W-OPEN-DT TO BA-LAST-POST-DT
           MOVE 0 TO BA-LAST-TXN
           MOVE 0 TO BA-NSF-CNT
           MOVE 0 TO BA-FEE-CNT
           MOVE 0 TO BA-CLOSE-DT
           WRITE BAL-REC
           IF FS-BAL NOT = "00"
               DISPLAY "   WARNING balance write FS=" FS-BAL " key=" BA-NO
           END-IF
           ADD AC-LTD-CRT TO W-TOT-DEP.

           *> ---- four loans across different products -----------------------
           *> Account and customer keys follow the scheme above.
       LOAD-LOANS.
           MOVE SPACES TO LOAN-REC
           MOVE "L00000000001" TO LN-NO
           MOVE "C000000002" TO LN-CUST-ID
           MOVE "1000200000" TO LN-DISB-ACCT
           MOVE "A" TO LN-TYPE
           MOVE "A" TO LN-STATUS
           MOVE 20220315 TO LN-OPEN-DT
           MOVE 20270315 TO LN-MATURITY-DT
           MOVE 20260915 TO LN-LAST-PMNT-DT
           MOVE 28500 TO LN-ORIGINAL
           MOVE 0.0599 TO LN-RATE
           MOVE 60 TO LN-TERM-MO
           MOVE 530.42 TO LN-PAYMENT
           MOVE 15 TO LN-DUE-DAY
           MOVE 1034 TO LN-BRANCH
           MOVE "AUTOMOBILE - 2022 MITSUBISHI" TO LN-NMEMO
           WRITE LOAN-REC
           MOVE SPACES TO LNBAL-REC
           MOVE "L00000000001" TO LB-NO
           MOVE 26200 TO LB-CUR-PRIN
           MOVE 145 TO LB-CUR-INT
           MOVE 512 TO LB-ACCR-INT-YTD
           MOVE 2300 TO LB-PAID-PRIN
           MOVE 388 TO LB-PAID-INT
           MOVE 24 TO LB-PMTS-MADE
           MOVE 20260915 TO LB-LAST-PMNT-DT
           MOVE 20261015 TO LB-NEXT-DUE-DT
           MOVE "0" TO LB-DELINQ
           MOVE 0 TO LB-DAYS-PAST-DUE
           MOVE 20260930 TO LB-LAST-ACC-DT
           WRITE LNBAL-REC

           MOVE SPACES TO LOAN-REC
           MOVE "L00000000002" TO LN-NO
           MOVE "C000000003" TO LN-CUST-ID
           MOVE "1000300000" TO LN-DISB-ACCT
           MOVE "M" TO LN-TYPE
           MOVE "A" TO LN-STATUS
           MOVE 20210401 TO LN-OPEN-DT
           MOVE 20510401 TO LN-MATURITY-DT
           MOVE 20260901 TO LN-LAST-PMNT-DT
           MOVE 385000 TO LN-ORIGINAL
           MOVE 0.05125 TO LN-RATE
           MOVE 360 TO LN-TERM-MO
           MOVE 2145.18 TO LN-PAYMENT
           MOVE 1 TO LN-DUE-DAY
           MOVE 1051 TO LN-BRANCH
           MOVE "RESIDENTIAL MORTGAGE - 40 YEAR" TO LN-NMEMO
           WRITE LOAN-REC
           MOVE SPACES TO LNBAL-REC
           MOVE "L00000000002" TO LB-NO
           MOVE 370415 TO LB-CUR-PRIN
           MOVE 1420 TO LB-CUR-INT
           MOVE 9860 TO LB-ACCR-INT-YTD
           MOVE 14585 TO LB-PAID-PRIN
           MOVE 21340 TO LB-PAID-INT
           MOVE 66 TO LB-PMTS-MADE
           MOVE 20260901 TO LB-LAST-PMNT-DT
           MOVE 20261001 TO LB-NEXT-DUE-DT
           MOVE "0" TO LB-DELINQ
           MOVE 0 TO LB-DAYS-PAST-DUE
           MOVE 20260930 TO LB-LAST-ACC-DT
           WRITE LNBAL-REC

           MOVE SPACES TO LOAN-REC
           MOVE "L00000000003" TO LN-NO
           MOVE "C000000007" TO LN-CUST-ID
           MOVE "1000700000" TO LN-DISB-ACCT
           MOVE "P" TO LN-TYPE
           MOVE "A" TO LN-STATUS
           MOVE 20251101 TO LN-OPEN-DT
           MOVE 20291101 TO LN-MATURITY-DT
           MOVE 20260910 TO LN-LAST-PMNT-DT
           MOVE 9500 TO LN-ORIGINAL
           MOVE 0.1175 TO LN-RATE
           MOVE 48 TO LN-TERM-MO
           MOVE 229.44 TO LN-PAYMENT
           MOVE 10 TO LN-DUE-DAY
           MOVE 1119 TO LN-BRANCH
           MOVE "PERSONAL LOAN - CONSOLIDATION" TO LN-NMEMO
           WRITE LOAN-REC
           MOVE SPACES TO LNBAL-REC
           MOVE "L00000000003" TO LB-NO
           MOVE 8200 TO LB-CUR-PRIN
           MOVE 190 TO LB-CUR-INT
           MOVE 890 TO LB-ACCR-INT-YTD
           MOVE 1300 TO LB-PAID-PRIN
           MOVE 240 TO LB-PAID-INT
           MOVE 11 TO LB-PMTS-MADE
           MOVE 20260910 TO LB-LAST-PMNT-DT
           MOVE 20261010 TO LB-NEXT-DUE-DT
           MOVE "1" TO LB-DELINQ
           MOVE 26 TO LB-DAYS-PAST-DUE
           MOVE 20260930 TO LB-LAST-ACC-DT
           WRITE LNBAL-REC

           MOVE SPACES TO LOAN-REC
           MOVE "L00000000004" TO LN-NO
           MOVE "C000000008" TO LN-CUST-ID
           MOVE "1000800000" TO LN-DISB-ACCT
           MOVE "C" TO LN-TYPE
           MOVE "A" TO LN-STATUS
           MOVE 20230101 TO LN-OPEN-DT
           MOVE 20330101 TO LN-MATURITY-DT
           MOVE 20260920 TO LN-LAST-PMNT-DT
           MOVE 12500 TO LN-ORIGINAL
           MOVE 0.1899 TO LN-RATE
           MOVE 120 TO LN-TERM-MO
           MOVE 279.63 TO LN-PAYMENT
           MOVE 20 TO LN-DUE-DAY
           MOVE 1136 TO LN-BRANCH
           MOVE "CARD - REVOLVING LINE" TO LN-NMEMO
           WRITE LOAN-REC
           MOVE SPACES TO LNBAL-REC
           MOVE "L00000000004" TO LB-NO
           MOVE 6420 TO LB-CUR-PRIN
           MOVE 240 TO LB-CUR-INT
           MOVE 1720 TO LB-ACCR-INT-YTD
           MOVE 6080 TO LB-PAID-PRIN
           MOVE 2980 TO LB-PAID-INT
           MOVE 48 TO LB-PMTS-MADE
           MOVE 20260920 TO LB-LAST-PMNT-DT
           MOVE 20261020 TO LB-NEXT-DUE-DT
           MOVE "0" TO LB-DELINQ
           MOVE 0 TO LB-DAYS-PAST-DUE
           MOVE 20260930 TO LB-LAST-ACC-DT
           WRITE LNBAL-REC.

       LOAD-TELLERS.
           MOVE SPACES TO TELLER-REC
           MOVE "TELLER01" TO TE-ID
           MOVE "OKONKWO, DANIEL A" TO TE-NAME
           MOVE "SUPVSR" TO TE-PASSWD
           MOVE 3 TO TE-LEVEL
           MOVE "A" TO TE-STATUS
           MOVE 1000 TO TE-BRANCH
           MOVE 25000 TO TE-LIMIT
           MOVE 10000 TO TE-CASH-LIMIT
           MOVE 0 TO TE-LAST-DT TE-LOCK-CNT
           WRITE TELLER-REC
           MOVE SPACES TO TELLER-REC
           MOVE "TELLER02" TO TE-ID
           MOVE "VASQUEZ, LINNETTE" TO TE-NAME
           MOVE "TELLER" TO TE-PASSWD
           MOVE 2 TO TE-LEVEL
           MOVE "A" TO TE-STATUS
           MOVE 1000 TO TE-BRANCH
           MOVE 5000 TO TE-LIMIT
           MOVE 2000 TO TE-CASH-LIMIT
           MOVE 0 TO TE-LAST-DT TE-LOCK-CNT
           WRITE TELLER-REC
           MOVE SPACES TO TELLER-REC
           MOVE "TELLER03" TO TE-ID
           MOVE "LINDQVIST, ANDERS" TO TE-NAME
           MOVE "CLERK" TO TE-PASSWD
           MOVE 1 TO TE-LEVEL
           MOVE "A" TO TE-STATUS
           MOVE 1000 TO TE-BRANCH
           MOVE 500 TO TE-LIMIT
           MOVE 200 TO TE-CASH-LIMIT
           MOVE 0 TO TE-LAST-DT TE-LOCK-CNT
           WRITE TELLER-REC
           MOVE SPACES TO TELLER-REC
           MOVE "TELLER09" TO TE-ID
           MOVE "DISABLED, SAMPLE" TO TE-NAME
           MOVE "GONE" TO TE-PASSWD
           MOVE 2 TO TE-LEVEL
           MOVE "X" TO TE-STATUS
           MOVE 1000 TO TE-BRANCH
           MOVE 5000 TO TE-LIMIT
           MOVE 2000 TO TE-CASH-LIMIT
           MOVE 0 TO TE-LAST-DT TE-LOCK-CNT
           WRITE TELLER-REC
           IF FS-TELL NOT = "00"
               DISPLAY "   WARNING teller write FS=" FS-TELL
           END-IF.

       LOAD-PARAMETERS.
           MOVE "NSF-FEE" TO PRM-NAME
           MOVE 25.00 TO PRM-VALUE
           MOVE "RETURNED ITEM CHARGE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "ODLIM-C" TO PRM-NAME
           MOVE 100.00 TO PRM-VALUE
           MOVE "CHECKING OVERDRAFT LIMIT" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "ODLIM-S" TO PRM-NAME
           MOVE 0 TO PRM-VALUE
           MOVE "SAVINGS OVERDRAFT LIMIT" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "MINBAL-C" TO PRM-NAME
           MOVE 25.00 TO PRM-VALUE
           MOVE "CHECKING MINIMUM BALANCE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "MINBAL-S" TO PRM-NAME
           MOVE 500.00 TO PRM-VALUE
           MOVE "SAVINGS MINIMUM BALANCE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "RATE-C" TO PRM-NAME
           MOVE 0.005 TO PRM-VALUE
           MOVE "CHECKING INTEREST RATE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "RATE-S" TO PRM-NAME
           MOVE 0.0425 TO PRM-VALUE
           MOVE "SAVINGS INTEREST RATE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "SVC-FEE" TO PRM-NAME
           MOVE 12.00 TO PRM-VALUE
           MOVE "MONTHLY SERVICE FEE" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "LOCK-MAX-MIN" TO PRM-NAME
           MOVE 5 TO PRM-VALUE
           MOVE "STALE RECORD LOCK, MINUTES" TO PRM-TEXT
           WRITE PARM-REC
           MOVE "INT-BASIS" TO PRM-NAME
           MOVE 365 TO PRM-VALUE
           MOVE "DAY COUNT BASIS" TO PRM-TEXT
           WRITE PARM-REC
           IF FS-PARM NOT = "00"
               DISPLAY "   WARNING parameter write FS=" FS-PARM
           END-IF.

       INIT-COUNT.
           MOVE SPACES TO SEQ-REC
           MOVE "TXNSEQ" TO SQ-NAME
           MOVE 0 TO SQ-VALUE
           WRITE SEQ-REC
           MOVE SPACES TO SEQ-REC
           MOVE "AUDSEQ" TO SQ-NAME
           MOVE 0 TO SQ-VALUE
           WRITE SEQ-REC
           MOVE SPACES TO SEQ-REC
           MOVE "GLSEQ" TO SQ-NAME
           MOVE 0 TO SQ-VALUE
           WRITE SEQ-REC
           MOVE SPACES TO SEQ-REC
           MOVE "JBRUN" TO SQ-NAME
           MOVE 0 TO SQ-VALUE
           WRITE SEQ-REC
           IF FS-SEQ NOT = "00"
               DISPLAY "   WARNING counter write FS=" FS-SEQ
           END-IF.

             *> ---- opening ledger position ----------------------------------
             *> A conversion has to bring the ledger in balance, so the
             *> deposits booked by this load are posted to the control
             *> accounts as one pair of conversion entries.
       OPEN-LEDGER.
           MOVE SPACES TO GLB-REC
           MOVE GA-CASH TO GB-ACCT
           MOVE GAN-CASH TO GB-NAME
           MOVE W-TOT-DEP TO GB-DEBITS
           MOVE 0 TO GB-CREDITS
           MOVE W-TOT-DEP TO GB-BALANCE
           WRITE GLB-REC
           MOVE SPACES TO GLB-REC
           MOVE GA-DEP TO GB-ACCT
           MOVE GAN-DEP TO GB-NAME
           MOVE 0 TO GB-DEBITS
           MOVE W-TOT-DEP TO GB-CREDITS
           COMPUTE GB-BALANCE = 0 - W-TOT-DEP
           WRITE GLB-REC
           MOVE SPACES TO GLB-REC
           MOVE GA-LOAN TO GB-ACCT
           MOVE GAN-LOAN TO GB-NAME
           MOVE 0 TO GB-DEBITS GB-CREDITS GB-BALANCE
           WRITE GLB-REC
           MOVE SPACES TO GLB-REC
           MOVE GA-INTEXP TO GB-ACCT
           MOVE GAN-INTEXP TO GB-NAME
           MOVE 0 TO GB-DEBITS GB-CREDITS GB-BALANCE
           WRITE GLB-REC
           MOVE SPACES TO GLB-REC
           MOVE GA-FEEINC TO GB-ACCT
           MOVE GAN-FEEINC TO GB-NAME
           MOVE 0 TO GB-DEBITS GB-CREDITS GB-BALANCE
           WRITE GLB-REC
           MOVE SPACES TO GLB-REC
           MOVE GA-CTRL TO GB-ACCT
           MOVE GAN-CTRL TO GB-NAME
           MOVE W-TOT-DEP TO GB-DEBITS
           MOVE W-TOT-DEP TO GB-CREDITS
           MOVE 0 TO GB-BALANCE
           WRITE GLB-REC
           IF FS-GLB NOT = "00"
               DISPLAY "   WARNING ledger balance write FS=" FS-GLB
           END-IF
             *> the matching entry lines, so the entry file and the balances
             *> tell the same story
           ACCEPT W-OPEN-DT FROM DATE YYYYMMDD
           MOVE 1 TO GL-SEQ
           MOVE W-OPEN-DT TO GL-DATE
           MOVE "MBLD001" TO GL-PROG
           MOVE "SYSTEM" TO GL-USER
           MOVE GA-CASH TO GL-ACCT
           MOVE "D" TO GL-SIDE
           MOVE "CONVERSION" TO GL-SUBACCT
           MOVE W-TOT-DEP TO GL-AMOUNT
           MOVE "CONVERSION - OPENING DEPOSITS" TO GL-DESCR
           WRITE GL-REC
           MOVE 2 TO GL-SEQ
           MOVE GA-DEP TO GL-ACCT
           MOVE "C" TO GL-SIDE
           MOVE W-TOT-DEP TO GL-AMOUNT
           MOVE "CONVERSION - OPENING DEPOSITS" TO GL-DESCR
           WRITE GL-REC
           IF FS-GL NOT = "00"
               DISPLAY "   WARNING ledger entry write FS=" FS-GL
           END-IF.
