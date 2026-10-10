      *> ====================================================================
      *> CUSTREC - Customer Master (REGISTRATION)  DSORG=Indexed KSDS
      *> ====================================================================
       01  CUST-REC.
           05  CU-ID            PIC X(10).
           05  CU-NAME          PIC X(30).
           05  CU-ADDR          PIC X(40).
           05  CU-CITY          PIC X(20).
           05  CU-STATE         PIC X(2).
           05  CU-ZIP           PIC X(9).
           05  CU-PHONE         PIC X(14).
           05  CU-SSN           PIC X(9).
           05  CU-DOB           PIC 9(8).
           05  CU-STATUS        PIC X(1).
           05  CU-OPEN-DT       PIC 9(8).
           05  CU-FIRST-DT      PIC 9(8).
           05  CU-CLOSE-DT      PIC 9(8).
           05  CU-BRANCH        PIC 9(4).
           05  CU-RISK          PIC X(1).
           05  CU-NMEMO         PIC X(50).
           05  FILLER           PIC X(8).
