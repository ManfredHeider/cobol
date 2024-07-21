      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25/01/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0529.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/CTKI0529.CPv  $
      *
      *   Rev 2.2   Jan 22 2020 10:50:12   t13ex39
      *AKI01754: Investment D: Neues Datenfeld und Fixselektion - U
      *ltimosparen 
      *
      *   Rev 2.1   Mar 18 2019 17:50:16   t13ae52
      *AKI01658: G48 - Kontokorrent Sourcen/COPYs
      *
      *   Rev 2.0   Jan 25 2019 13:06:39   T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_KONTOKORRENT TABLE
CCC004*    ( G48KKOVNR                      CHAR(15) NOT NULL,
CCC004*      G48KKOUKONTONR                 CHAR(2) NOT NULL,
CCC004*      G48KKOWAEHRUNG                 CHAR(3) NOT NULL,
CCC004*      G48KKODATOEFF                  CHAR(8) NOT NULL,
CCC004*      G48KKOSCHLDAT                  CHAR(8) NOT NULL,
CCC004*      G48KKOBILANZSCHL               CHAR(4) NOT NULL,
CCC004*      G48KKOEXTLIMIT                 DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOSALDO                    DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKONDSCHL                 CHAR(2) NOT NULL,
CCC004*      G48KKODELKZ                    CHAR(1) NOT NULL,
CCC004*      G48KKOKUMHABENAM               DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKUMSOLLAM                DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKUMHABENVM               DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKUMSOLLVM                DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOZRAM                     CHAR(5) NOT NULL,
CCC004*      G48KKOZRVM                     CHAR(5) NOT NULL,
CCC004*      G48KKOSOLLZINS                 DECIMAL(6, 4) NOT NULL,
CCC004*      G48KKOUEBERZINS                DECIMAL(6, 4) NOT NULL,
CCC004*      G48KKOHABENZINS                DECIMAL(6, 4) NOT NULL,
CCC004*      G48KKODATENDEBEG               CHAR(8) NOT NULL,
CCC004*      G48KKOIBAN                     CHAR(34) NOT NULL,
CCC004*      G48KKOBIC                      CHAR(11) NOT NULL,
CCC004*      G48KKOMANDREF                  CHAR(35) NOT NULL,
CCC004*      G48KKOINHABER                  CHAR(28) NOT NULL,
CCC004*      G48KKOIBANKK                   CHAR(34) NOT NULL,
CCC004*      G48KKOKENNZSCHL                CHAR(2) NOT NULL,
CCC004*      G48KKODATSALDO                 CHAR(8) NOT NULL,
CCC004*      G48KKOKOSTSCHL                 CHAR(1) NOT NULL,
CCC004*      G48KKOPIN                      CHAR(9) NOT NULL,
CCC004*      G48KKOKFWERSTR                 DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKFWFOLGER                DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKFWKREDITB               DECIMAL(14, 2) NOT NULL,
CCC004*      G48KKOKFWSOLLZ                 DECIMAL(6, 4) NOT NULL,
CCC004*      G48KKOKFWDATFEST               CHAR(8) NOT NULL,
CCC004*      G48KKOKFWTILGUNG               DECIMAL(6, 4) NOT NULL,
CCC004*      G48KKOULTOSPARKZ               CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_KONTOKORRENT            *
      ******************************************************************
       01  G48-KONTOKORRENT-GESAMT.
        05 NI-G48-KONTOKORRENT-TAB.
           10 NI-G48-KONTOKORRENT-ELE PIC S9(04) COMP OCCURS 36 TIMES.
        05 NI-G48-KONTOKORRENT REDEFINES NI-G48-KONTOKORRENT-TAB.
           10 NI-G48KKOVNR          PIC S9(4) COMP.
           10 NI-G48KKOUKONTONR     PIC S9(4) COMP.
           10 NI-G48KKOWAEHRUNG     PIC S9(4) COMP.
           10 NI-G48KKODATOEFF      PIC S9(4) COMP.
           10 NI-G48KKOSCHLDAT      PIC S9(4) COMP.
           10 NI-G48KKOBILANZSCHL   PIC S9(4) COMP.
           10 NI-G48KKOEXTLIMIT     PIC S9(4) COMP.
           10 NI-G48KKOSALDO        PIC S9(4) COMP.
           10 NI-G48KKOKONDSCHL     PIC S9(4) COMP.
           10 NI-G48KKODELKZ        PIC S9(4) COMP.
           10 NI-G48KKOKUMHABENAM   PIC S9(4) COMP.
           10 NI-G48KKOKUMSOLLAM    PIC S9(4) COMP.
           10 NI-G48KKOKUMHABENVM   PIC S9(4) COMP.
           10 NI-G48KKOKUMSOLLVM    PIC S9(4) COMP.
           10 NI-G48KKOZRAM         PIC S9(4) COMP.
           10 NI-G48KKOZRVM         PIC S9(4) COMP.
           10 NI-G48KKOSOLLZINS     PIC S9(4) COMP.
           10 NI-G48KKOUEBERZINS    PIC S9(4) COMP.
           10 NI-G48KKOHABENZINS    PIC S9(4) COMP.
           10 NI-G48KKODATENDEBEG   PIC S9(4) COMP.
           10 NI-G48KKOIBAN         PIC S9(4) COMP.
           10 NI-G48KKOBIC          PIC S9(4) COMP.
           10 NI-G48KKOMANDREF      PIC S9(4) COMP.
           10 NI-G48KKOINHABER      PIC S9(4) COMP.
           10 NI-G48KKOIBANKK       PIC S9(4) COMP.
           10 NI-G48KKOKENNZSCHL    PIC S9(4) COMP.
           10 NI-G48KKODATSALDO     PIC S9(4) COMP.
           10 NI-G48KKOKOSTSCHL     PIC S9(4) COMP.
           10 NI-G48KKOPIN          PIC S9(4) COMP.
           10 NI-G48KKOKFWERSTR     PIC S9(4) COMP.
           10 NI-G48KKOKFWFOLGER    PIC S9(4) COMP.
           10 NI-G48KKOKFWKREDITB   PIC S9(4) COMP.
           10 NI-G48KKOKFWSOLLZ     PIC S9(4) COMP.
           10 NI-G48KKOKFWDATFEST   PIC S9(4) COMP.
           10 NI-G48KKOKFWTILGUNG   PIC S9(4) COMP.
           10 NI-G48KKOULTOSPARKZ   PIC S9(4) COMP.
        05 DCL-G48_KONTOKORRENT.
           10 G48KKOVNR            PIC X(15).
           10 G48KKOUKONTONR       PIC X(2).
           10 G48KKOWAEHRUNG       PIC X(3).
           10 G48KKODATOEFF        PIC X(8).
           10 G48KKOSCHLDAT        PIC X(8).
           10 G48KKOBILANZSCHL     PIC X(4).
           10 G48KKOEXTLIMIT       PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOSALDO          PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKONDSCHL       PIC X(2).
           10 G48KKODELKZ          PIC X(1).
           10 G48KKOKUMHABENAM     PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKUMSOLLAM      PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKUMHABENVM     PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKUMSOLLVM      PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOZRAM           PIC X(5).
           10 G48KKOZRVM           PIC X(5).
           10 G48KKOSOLLZINS       PIC S9(2)V9(4) USAGE COMP-3.
           10 G48KKOUEBERZINS      PIC S9(2)V9(4) USAGE COMP-3.
           10 G48KKOHABENZINS      PIC S9(2)V9(4) USAGE COMP-3.
           10 G48KKODATENDEBEG     PIC X(8).
           10 G48KKOIBAN           PIC X(34).
           10 G48KKOBIC            PIC X(11).
           10 G48KKOMANDREF        PIC X(35).
           10 G48KKOINHABER        PIC X(28).
           10 G48KKOIBANKK         PIC X(34).
           10 G48KKOKENNZSCHL      PIC X(2).
           10 G48KKODATSALDO       PIC X(8).
           10 G48KKOKOSTSCHL       PIC X(1).
           10 G48KKOPIN            PIC X(9).
           10 G48KKOKFWERSTR       PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKFWFOLGER      PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKFWKREDITB     PIC S9(12)V9(2) USAGE COMP-3.
           10 G48KKOKFWSOLLZ       PIC S9(2)V9(4) USAGE COMP-3.
           10 G48KKOKFWDATFEST     PIC X(8).
           10 G48KKOKFWTILGUNG     PIC S9(2)V9(4) USAGE COMP-3.
           10 G48KKOULTOSPARKZ     PIC X(1).
      ************************** ENDE-DCLGEN *************************

