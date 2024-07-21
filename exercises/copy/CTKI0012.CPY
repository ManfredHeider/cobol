      ******************************************************************
      * DCLGEN TABLE                                                   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE  BERUFSGRUPPE  TABLE
CCC004*    ( BERUFSGRUPPEID          SMALLINT NOT NULL,
CCC004*      BERUFSGRUPPETEXT        CHAR (20) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  BERUFSGRUPPE-GESAMT.
        05  NI-BERUFSGRUPPE-TAB.
           10 NI-BERUFSGRUPPE-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05  NI-BERUFSGRUPPE  REDEFINES  NI-BERUFSGRUPPE-TAB.
           10 NI-BERUFSGRUPPEID           PIC S9(4) COMP.
           10 NI-BERUFSGRUPPETEXT         PIC S9(4) COMP.
        05  DCL-BERUFSGRUPPE.
           10 BERUFSGRUPPEID              PIC S9(4) COMP.
           10 BERUFSGRUPPETEXT            PIC X(20).
      ******************************************************************
