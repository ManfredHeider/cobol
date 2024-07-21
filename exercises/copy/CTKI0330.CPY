      *****************************************************************
CCC005*    EXEC SQL DECLARE LEBENTEILSCHICHT TABLE
CCC004*    ( LTSVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      LTSVERSTARLFDNR                SMALLINT NOT NULL,
CCC004*      LTSZAHLDYNERHOEH               SMALLINT NOT NULL,
CCC004*      LTSTTNR                        SMALLINT NOT NULL,
CCC004*      LTSDATDYNERHOEH                DATE,
CCC004*      LTSSCHICHTBEITR                DECIMAL(13, 2),
CCC004*      LTSWKZ                         CHAR(1) NOT NULL,
CCC004*      LTSVERSSUMME                   DECIMAL(13, 2),
CCC004*      LTSTARBEZEICHNUNG              CHAR(15),
CCC004*      LTSBEITRZUSTID                 CHAR(4),
CCC004*      LTSVERTRZUSTANDID              SMALLINT,
CCC004*      LTSVERTRZUSTANDDAT             DATE,
CCC004*      LETZTEAENDERUNG                TIMESTAMP,
CCC004*      LTSSCHICHTZUGARTID             CHAR(1) NOT NULL,
CCC004*      LTSTTYPBEITR                   DECIMAL(13, 2),
CCC004*      LTSDEPZINSUEBANT               DECIMAL(5, 2),
CCC004*      LTSRNTSCHL                     CHAR(1),
CCC004*      LTSRNTSCHLDAT                  DATE,
CCC004*      LTSBTGRUEKZNPFL                CHAR(1) NOT NULL,
CCC004*      LTSKLBERHAUSSCHL               CHAR(1) NOT NULL,
CCC004*      LTSRABSTUFE                    CHAR(3)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE LEBENTEILSCHICHT            *
      ******************************************************************
       01  LEBENTEILSCHICHT-GESAMT.
        05 NI-LEBENTEILSCHICHT-TAB.
           10 NI-LEBENTEILSCHICHT-ELE PIC S9(04) COMP OCCURS 21 TIMES.
        05 NI-LEBENTEILSCHICHT REDEFINES NI-LEBENTEILSCHICHT-TAB.
           10 NI-LTSVERTRAGSID      PIC S9(4) COMP.
           10 NI-LTSVERSTARLFDNR    PIC S9(4) COMP.
           10 NI-LTSZAHLDYNERHOEH   PIC S9(4) COMP.
           10 NI-LTSTTNR            PIC S9(4) COMP.
           10 NI-LTSDATDYNERHOEH    PIC S9(4) COMP.
           10 NI-LTSSCHICHTBEITR    PIC S9(4) COMP.
           10 NI-LTSWKZ             PIC S9(4) COMP.
           10 NI-LTSVERSSUMME       PIC S9(4) COMP.
           10 NI-LTSTARBEZEICHNUNG  PIC S9(4) COMP.
           10 NI-LTSBEITRZUSTID     PIC S9(4) COMP.
           10 NI-LTSVERTRZUSTANDID  PIC S9(4) COMP.
           10 NI-LTSVERTRZUSTANDDAT PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-LTSSCHICHTZUGARTID PIC S9(4) COMP.
           10 NI-LTSTTYPBEITR       PIC S9(4) COMP.
           10 NI-LTSDEPZINSUEBANT   PIC S9(4) COMP.
           10 NI-LTSRNTSCHL         PIC S9(4) COMP.
           10 NI-LTSRNTSCHLDAT      PIC S9(4) COMP.
           10 NI-LTSBTGRUEKZNPFL    PIC S9(4) COMP.
           10 NI-LTSKLBERHAUSSCHL   PIC S9(4) COMP.
           10 NI-LTSRABSTUFE        PIC S9(4) COMP.
        05 DCL-LEBENTEILSCHICHT.
           10 LTSVERTRAGSID        PIC X(13).
           10 LTSVERSTARLFDNR      PIC S9(4) USAGE COMP-5.
           10 LTSZAHLDYNERHOEH     PIC S9(4) USAGE COMP-5.
           10 LTSTTNR              PIC S9(4) USAGE COMP-5.
           10 LTSDATDYNERHOEH      PIC X(10).
           10 LTSSCHICHTBEITR      PIC S9(11)V9(2) USAGE COMP-3.
           10 LTSWKZ               PIC X(1).
           10 LTSVERSSUMME         PIC S9(11)V9(2) USAGE COMP-3.
           10 LTSTARBEZEICHNUNG    PIC X(15).
           10 LTSBEITRZUSTID       PIC X(4).
           10 LTSVERTRZUSTANDID    PIC S9(4) USAGE COMP-5.
           10 LTSVERTRZUSTANDDAT   PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
           10 LTSSCHICHTZUGARTID   PIC X(1).
           10 LTSTTYPBEITR         PIC S9(11)V9(2) USAGE COMP-3.
           10 LTSDEPZINSUEBANT     PIC S9(3)V9(2) USAGE COMP-3.
           10 LTSRNTSCHL           PIC X(1).
           10 LTSRNTSCHLDAT        PIC X(10).
           10 LTSBTGRUEKZNPFL      PIC X(1).
           10 LTSKLBERHAUSSCHL     PIC X(1).
           10 LTSRABSTUFE          PIC X(3).
      ************************** ENDE-DCLGEN *************************
