      *****************************************************************
CCC005*    EXEC SQL DECLARE G48_BAUFI TABLE
CCC004*    ( G48BFVNR                       CHAR(15) NOT NULL,
CCC004*      G48BFUKTONR                    CHAR(2) NOT NULL,
CCC004*      G48BFWAEHRUNG                  CHAR(3) NOT NULL,
CCC004*      G48BFOEFDAT                    CHAR(8) NOT NULL,
CCC004*      G48BFENDEDAT                   CHAR(8) NOT NULL,
CCC004*      G48BFDARLEHEN                  DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFKREDITZUSDAT              CHAR(8) NOT NULL,
CCC004*      G48BFKREDITAUSDAT              CHAR(8) NOT NULL,
CCC004*      G48BFENDEZINSDAT               CHAR(8) NOT NULL,
CCC004*      G48BFAKTUSALDO                 DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFENDEZRFREI                CHAR(8) NOT NULL,
CCC004*      G48BFTILGUNGSART               CHAR(1) NOT NULL,
CCC004*      G48BFTILGUNGPROZ               DECIMAL(6, 4) NOT NULL,
CCC004*      G48BFTILGUNGPROD               CHAR(10) NOT NULL,
CCC004*      G48BFTILGUNGBEITR              DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFSOLLZINS                  DECIMAL(6, 4) NOT NULL,
CCC004*      G48BFUEBERZINS                 DECIMAL(6, 4) NOT NULL,
CCC004*      G48BFZAHLUNGSW                 CHAR(1) NOT NULL,
CCC004*      G48BFANNUITAET                 DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFRATEANT                   DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFZINSANT                   DECIMAL(14, 2) NOT NULL,
CCC004*      G48BFDELKZ                     CHAR(1) NOT NULL,
CCC004*      G48BFIBAN                      CHAR(34) NOT NULL,
CCC004*      G48BFBIC                       CHAR(11) NOT NULL,
CCC004*      G48BFMANDREF                   CHAR(35) NOT NULL,
CCC004*      G48BFNNAMEBZ                   CHAR(28) NOT NULL,
CCC004*      G48BFIBANBF                    CHAR(34) NOT NULL,
CCC004*      G48BFZINSART                   CHAR(2) NOT NULL,
CCC004*      G48BFORWORDKZ                  CHAR(1) NOT NULL,
CCC004*      G48BFPIN                       INTEGER NOT NULL,
CCC004*      G48BFVOLLAUSZDAT               CHAR(8) NOT NULL,
CCC004*      LETZTERAENDERER                CHAR(10) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      G48BFFAELLIGDAT                CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_BAUFI                   *
      ******************************************************************
       01  G48-BAUFI-GESAMT.
        05 NI-G48-BAUFI-TAB.
           10 NI-G48-BAUFI-ELE PIC S9(04) COMP OCCURS 34 TIMES.
        05 NI-G48-BAUFI REDEFINES NI-G48-BAUFI-TAB.
           10 NI-G48BFVNR           PIC S9(4) COMP.
           10 NI-G48BFUKTONR        PIC S9(4) COMP.
           10 NI-G48BFWAEHRUNG      PIC S9(4) COMP.
           10 NI-G48BFOEFDAT        PIC S9(4) COMP.
           10 NI-G48BFENDEDAT       PIC S9(4) COMP.
           10 NI-G48BFDARLEHEN      PIC S9(4) COMP.
           10 NI-G48BFKREDITZUSDAT  PIC S9(4) COMP.
           10 NI-G48BFKREDITAUSDAT  PIC S9(4) COMP.
           10 NI-G48BFENDEZINSDAT   PIC S9(4) COMP.
           10 NI-G48BFAKTUSALDO     PIC S9(4) COMP.
           10 NI-G48BFENDEZRFREI    PIC S9(4) COMP.
           10 NI-G48BFTILGUNGSART   PIC S9(4) COMP.
           10 NI-G48BFTILGUNGPROZ   PIC S9(4) COMP.
           10 NI-G48BFTILGUNGPROD   PIC S9(4) COMP.
           10 NI-G48BFTILGUNGBEITR  PIC S9(4) COMP.
           10 NI-G48BFSOLLZINS      PIC S9(4) COMP.
           10 NI-G48BFUEBERZINS     PIC S9(4) COMP.
           10 NI-G48BFZAHLUNGSW     PIC S9(4) COMP.
           10 NI-G48BFANNUITAET     PIC S9(4) COMP.
           10 NI-G48BFRATEANT       PIC S9(4) COMP.
           10 NI-G48BFZINSANT       PIC S9(4) COMP.
           10 NI-G48BFDELKZ         PIC S9(4) COMP.
           10 NI-G48BFIBAN          PIC S9(4) COMP.
           10 NI-G48BFBIC           PIC S9(4) COMP.
           10 NI-G48BFMANDREF       PIC S9(4) COMP.
           10 NI-G48BFNNAMEBZ       PIC S9(4) COMP.
           10 NI-G48BFIBANBF        PIC S9(4) COMP.
           10 NI-G48BFZINSART       PIC S9(4) COMP.
           10 NI-G48BFORWORDKZ      PIC S9(4) COMP.
           10 NI-G48BFPIN           PIC S9(4) COMP.
           10 NI-G48BFVOLLAUSZDAT   PIC S9(4) COMP.
           10 NI-LETZTERAENDERER    PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-G48BFFAELLIGDAT    PIC S9(4) COMP.
        05 DCL-G48-BAUFI.
           10 G48BFVNR             PIC X(15).
           10 G48BFUKTONR          PIC X(2).
           10 G48BFWAEHRUNG        PIC X(3).
           10 G48BFOEFDAT          PIC X(8).
           10 G48BFENDEDAT         PIC X(8).
           10 G48BFDARLEHEN        PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFKREDITZUSDAT    PIC X(8).
           10 G48BFKREDITAUSDAT    PIC X(8).
           10 G48BFENDEZINSDAT     PIC X(8).
           10 G48BFAKTUSALDO       PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFENDEZRFREI      PIC X(8).
           10 G48BFTILGUNGSART     PIC X(1).
           10 G48BFTILGUNGPROZ     PIC S9(2)V9(4) USAGE COMP-3.
           10 G48BFTILGUNGPROD     PIC X(10).
           10 G48BFTILGUNGBEITR    PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFSOLLZINS        PIC S9(2)V9(4) USAGE COMP-3.
           10 G48BFUEBERZINS       PIC S9(2)V9(4) USAGE COMP-3.
           10 G48BFZAHLUNGSW       PIC X(1).
           10 G48BFANNUITAET       PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFRATEANT         PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFZINSANT         PIC S9(12)V9(2) USAGE COMP-3.
           10 G48BFDELKZ           PIC X(1).
           10 G48BFIBAN            PIC X(34).
           10 G48BFBIC             PIC X(11).
           10 G48BFMANDREF         PIC X(35).
           10 G48BFNNAMEBZ         PIC X(28).
           10 G48BFIBANBF          PIC X(34).
           10 G48BFZINSART         PIC X(2).
           10 G48BFORWORDKZ        PIC X(1).
           10 G48BFPIN             PIC S9(9) USAGE COMP.
           10 G48BFVOLLAUSZDAT     PIC X(8).
           10 LETZTERAENDERER      PIC X(10).
           10 LETZTEAENDERUNG      PIC X(26).
           10 G48BFFAELLIGDAT      PIC X(8).
      ************************** ENDE-DCLGEN *************************