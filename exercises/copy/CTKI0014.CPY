      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BUNDESLAND  TABLE
CCC004*    ( BUNDESLANDID            CHAR (3) NOT NULL,
CCC004*      BUNDESLANDLANDID        CHAR (3) NOT NULL,
CCC004*      BUNDESLANDTEXT          CHAR (30) NOT NULL,
CCC004*      BUNDESLANDKIST          DECIMAL (11, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BUNDESLAND-GESAMT.
        05  NI-BUNDESLAND-TAB.
           10 NI-BUNDESLAND-ELE PIC S9(04) COMP OCCURS 4 TIMES.
        05  NI-BUNDESLAND  REDEFINES  NI-BUNDESLAND-TAB.
           10 NI-BUNDESLANDID             PIC S9(4) COMP.
           10 NI-BUNDESLANDLANDID         PIC S9(4) COMP.
           10 NI-BUNDESLANDTEXT           PIC S9(4) COMP.
           10 NI-BUNDESLANDKIST           PIC S9(4) COMP.
        05  DCL-BUNDESLAND.
           10 BUNDESLANDID                PIC X(3).
           10 BUNDESLANDLANDID            PIC X(3).
           10 BUNDESLANDTEXT              PIC X(30).
           10 BUNDESLANDKIST              PIC S9(9)V9(2) COMP-3.
      ******************************************************************
