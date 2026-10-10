      *> ====================================================================
      *> SEQREC - Counter dataset. Indexed by counter name so every program
      *> can draw unique journal/audit sequence numbers without contention
      *> against a single in-memory counter.
      *> ====================================================================
       01  SEQ-REC.
           05  SQ-NAME          PIC X(8).
           05  SQ-VALUE         PIC 9(12) COMP-3.
           05  SQ-LAST-DT       PIC 9(8).
           05  FILLER            PIC X(6).
