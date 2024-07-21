      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06/07/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0498.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0498.CPv  $
      *
      *   Rev 2.3   Feb 13 2020 06:40:40   t13ex39
      *AKI01765: Deutsche Bank - Ergänzung fehlender Kreditkartenar
      *ten
      *
      *   Rev 2.2   Sep 13 2018 09:58:02   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.1   Sep 10 2018 09:50:46   t13ae52
      *AKI01601: Kreditkarten - Umstellung
      *
      *   Rev 2.0   Jul 6 2018 09:29:55   T13AE52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_KREDITKARTE TABLE
CCC004*    ( G48KKAVNR                      CHAR(15) NOT NULL,
CCC004*      G48KKAART                      CHAR(3) NOT NULL,
CCC004*      G48KKANR                       CHAR(16) NOT NULL,
CCC004*      G48KKAHAUPTNR                  CHAR(16) NOT NULL,
CCC004*      G48KKAVNRHAUPT                 CHAR(10) NOT NULL,
CCC004*      G48KKAABRECHKTO                CHAR(10) NOT NULL,
CCC004*      G48KKAABRECHBLZ                CHAR(8) NOT NULL,
CCC004*      G48KKAOEFFDAT                  CHAR(8) NOT NULL,
CCC004*      G48KKASCHLDAT                  CHAR(8) NOT NULL,
CCC004*      G48KKAMAHNSTATUS               CHAR(1) NOT NULL,
CCC004*      G48KKAFILNRKTO                 CHAR(3) NOT NULL,
CCC004*      G48KKAIBAN                     CHAR(34) NOT NULL,
CCC004*      G48KKABIC                      CHAR(11) NOT NULL,
CCC004*      G48KKAMANDREF                  CHAR(35) NOT NULL,
CCC004*      G48KKANACHNAMEBZ               CHAR(28) NOT NULL,
CCC004*      G48KKADELKZ                    CHAR(1) NOT NULL,
CCC004*      G48KKAPIN                      CHAR(9) NOT NULL,
CCC004*      G48KKALIMIT                    DECIMAL(8, 2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_KREDITKARTE             *
      ******************************************************************
       01  G48-KREDITKARTE-GESAMT.
        05 NI-G48-KREDITKARTE-TAB.
           10 NI-G48-KREDITKARTE-ELE PIC S9(04) COMP OCCURS 18 TIMES.
        05 NI-G48-KREDITKARTE REDEFINES NI-G48-KREDITKARTE-TAB.
           10 NI-G48KKAVNR          PIC S9(4) COMP.
           10 NI-G48KKAART          PIC S9(4) COMP.
           10 NI-G48KKANR           PIC S9(4) COMP.
           10 NI-G48KKAHAUPTNR      PIC S9(4) COMP.
           10 NI-G48KKAVNRHAUPT     PIC S9(4) COMP.
           10 NI-G48KKAABRECHKTO    PIC S9(4) COMP.
           10 NI-G48KKAABRECHBLZ    PIC S9(4) COMP.
           10 NI-G48KKAOEFFDAT      PIC S9(4) COMP.
           10 NI-G48KKASCHLDAT      PIC S9(4) COMP.
           10 NI-G48KKAMAHNSTATUS   PIC S9(4) COMP.
           10 NI-G48KKAFILNRKTO     PIC S9(4) COMP.
           10 NI-G48KKAIBAN         PIC S9(4) COMP.
           10 NI-G48KKABIC          PIC S9(4) COMP.
           10 NI-G48KKAMANDREF      PIC S9(4) COMP.
           10 NI-G48KKANACHNAMEBZ   PIC S9(4) COMP.
           10 NI-G48KKADELKZ        PIC S9(4) COMP.
           10 NI-G48KKAPIN          PIC S9(4) COMP.
           10 NI-G48KKALIMIT        PIC S9(4) COMP.
        05 DCL-G48-KREDITKARTE.
           10 G48KKAVNR            PIC X(15).
           10 G48KKAART            PIC X(3).
           10 G48KKANR             PIC X(16).
           10 G48KKAHAUPTNR        PIC X(16).
           10 G48KKAVNRHAUPT       PIC X(10).
           10 G48KKAABRECHKTO      PIC X(10).
           10 G48KKAABRECHBLZ      PIC X(8).
           10 G48KKAOEFFDAT        PIC X(8).
           10 G48KKASCHLDAT        PIC X(8).
           10 G48KKAMAHNSTATUS     PIC X(1).
           10 G48KKAFILNRKTO       PIC X(3).
           10 G48KKAIBAN           PIC X(34).
           10 G48KKABIC            PIC X(11).
           10 G48KKAMANDREF        PIC X(35).
           10 G48KKANACHNAMEBZ     PIC X(28).
           10 G48KKADELKZ          PIC X(1).
           10 G48KKAPIN            PIC X(9).
           10 G48KKALIMIT          PIC S9(6)V9(2) USAGE COMP-3.
      ************************** ENDE-DCLGEN *************************

