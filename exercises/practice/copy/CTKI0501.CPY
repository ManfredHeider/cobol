      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/02/15
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0501.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/CTKI0501.CPv  $
      *
      *   Rev 2.4   Jan 25 2018 06:55:06   t13ex39
      *AKI01556: DeuBa zusätzliche  PINs OZPs
      *
      *   Rev 2.2   Feb 18 2015 16:56:08   t13ex29
      *AKI01348: Auslagern EC-Karten in eig. Tabelle- Tabelle
      *
      *   Rev 2.0   Feb 11 2015 15:25:06   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE BANKECKARTE TABLE
CCC004*    ( BECVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      BECKONTOUNR                    SMALLINT NOT NULL,
CCC004*      BECLFDNR                       SMALLINT NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,
CCC004*      BECKARTENART                   CHAR(3),
CCC004*      BECANZKARTEN                   SMALLINT,
CCC004*      BECKARTENNR                    VARCHAR(16),
CCC004*      BECPIN                         INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE BANKECKARTE                 *
      ******************************************************************
       01  BANKECKARTE-GESAMT.
        05 NI-BANKECKARTE-TAB.
           10 NI-BANKECKARTE-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05 NI-BANKECKARTE REDEFINES NI-BANKECKARTE-TAB.
           10 NI-BECVERTRAGSID      PIC S9(4) COMP.
           10 NI-BECKONTOUNR        PIC S9(4) COMP.
           10 NI-BECLFDNR           PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
           10 NI-BECKARTENART       PIC S9(4) COMP.
           10 NI-BECANZKARTEN       PIC S9(4) COMP.
           10 NI-BECKARTENNR        PIC S9(4) COMP.
           10 NI-BECPIN             PIC S9(4) COMP.
        05 DCL-BANKECKARTE.
           10 BECVERTRAGSID        PIC X(13).
           10 BECKONTOUNR          PIC S9(4) USAGE COMP.
           10 BECLFDNR             PIC S9(4) USAGE COMP.
           10 LETZTEAENDERUNG      PIC X(26).
           10 BECKARTENART         PIC X(3).
           10 BECANZKARTEN         PIC S9(4) USAGE COMP.
           10 BECKARTENNR.
              49 BECKARTENNR-LEN   PIC S9(4) USAGE COMP.
              49 BECKARTENNR-TEXT
                 PIC X(16).
           10 BECPIN               PIC S9(9) USAGE COMP.
      ************************** ENDE-DCLGEN *************************

