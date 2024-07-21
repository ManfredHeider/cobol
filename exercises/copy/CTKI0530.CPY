      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15/03/19
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0530.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/CTKI0530.CPv  $
      *
      *   Rev 2.5   Jul 28 2020 14:16:30   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.4   Jul 20 2020 09:11:22   t13ex39
      *AKI01671: OZP
      *
      *   Rev 2.3   Apr 29 2019 14:57:28   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.2   Apr 29 2019 14:48:08   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.1   Apr 11 2019 13:22:56   i13az81
      *AKI01671: OZP
      *
      *   Rev 2.0   Mar 15 2019 12:46:02   I13AZ81
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_SPARDATEN TABLE
CCC004*    ( G48SPDVNR                      CHAR(15) NOT NULL,
CCC004*      G48SPDUKONTONR                 CHAR(2) NOT NULL,
CCC004*      G48SPDDATOEFF                  CHAR(8) NOT NULL,
CCC004*      G48SPDSCHLDAT                  CHAR(8) NOT NULL,
CCC004*      G48SPDKONTOART                 CHAR(3) NOT NULL,
CCC004*      G48SPDSALDO                    DECIMAL(14, 2) NOT NULL,
CCC004*      G48SPDVERENDE                  CHAR(8) NOT NULL,
CCC004*      G48SPDZAHLWEISE                CHAR(1) NOT NULL,
CCC004*      G48SPDRATENHOEHE               DECIMAL(14, 2) NOT NULL,
CCC004*      G48SPDHABENZINS                DECIMAL(6, 4) NOT NULL,
CCC004*      G48SPDDELKZ                    CHAR(1) NOT NULL,
CCC004*      G48SPDIBAN                     CHAR(34) NOT NULL,
CCC004*      G48SPDBIC                      CHAR(11) NOT NULL,
CCC004*      G48SPDMANDREF                  CHAR(34) NOT NULL,
CCC004*      G48SPDNACHNAMEBZ               CHAR(28) NOT NULL,
CCC004*      G48SPDIBANSPK                  CHAR(34) NOT NULL,
CCC004*      G48SPDPIN                      CHAR(9) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_SPARDATEN               *
      ******************************************************************
       01  G48-SPARDATEN-GESAMT.
        05 NI-G48-SPARDATEN-TAB.
           10 NI-G48-SPARDATEN-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-G48-SPARDATEN REDEFINES NI-G48-SPARDATEN-TAB.
           10 NI-G48SPDVNR          PIC S9(4) COMP.
           10 NI-G48SPDUKONTONR     PIC S9(4) COMP.
           10 NI-G48SPDDATOEFF      PIC S9(4) COMP.
           10 NI-G48SPDSCHLDAT      PIC S9(4) COMP.
           10 NI-G48SPDKONTOART     PIC S9(4) COMP.
           10 NI-G48SPDSALDO        PIC S9(4) COMP.
           10 NI-G48SPDVERENDE      PIC S9(4) COMP.
           10 NI-G48SPDZAHLWEISE    PIC S9(4) COMP.
           10 NI-G48SPDRATENHOEHE   PIC S9(4) COMP.
           10 NI-G48SPDHABENZINS    PIC S9(4) COMP.
           10 NI-G48SPDDELKZ        PIC S9(4) COMP.
           10 NI-G48SPDIBAN         PIC S9(4) COMP.
           10 NI-G48SPDBIC          PIC S9(4) COMP.
           10 NI-G48SPDMANDREF      PIC S9(4) COMP.
           10 NI-G48SPDNACHNAMEBZ   PIC S9(4) COMP.
           10 NI-G48SPDIBANSPK      PIC S9(4) COMP.
           10 NI-G48SPDPIN          PIC S9(4) COMP.
        05 DCL-G48_SPARDATEN.
           10 G48SPDVNR            PIC X(15).
           10 G48SPDUKONTONR       PIC X(2).
           10 G48SPDDATOEFF        PIC X(8).
           10 G48SPDSCHLDAT        PIC X(8).
           10 G48SPDKONTOART       PIC X(3).
           10 G48SPDSALDO          PIC S9(12)V9(2) USAGE COMP-3.
           10 G48SPDVERENDE        PIC X(8).
           10 G48SPDZAHLWEISE      PIC X(1).
           10 G48SPDRATENHOEHE     PIC S9(12)V9(2) USAGE COMP-3.
           10 G48SPDHABENZINS      PIC S9(2)V9(4) USAGE COMP-3.
           10 G48SPDDELKZ          PIC X(1).
           10 G48SPDIBAN           PIC X(34).
           10 G48SPDBIC            PIC X(11).
           10 G48SPDMANDREF        PIC X(34).
           10 G48SPDNACHNAMEBZ     PIC X(28).
           10 G48SPDIBANSPK        PIC X(34).
           10 G48SPDPIN            PIC X(9).
      ************************** ENDE-DCLGEN *************************





