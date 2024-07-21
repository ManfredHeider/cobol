      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02/07/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0497.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0497.CPv  $
      *
      *   Rev 2.1   Jan 12 2021 15:38:18   u130684
      *AKI01775: Neues Ermittlungsprogramm Fondsdepot Bank
      *
      *   Rev 2.0   Jul 2 2018 15:20:07   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G40_KUNDEVERTRAG TABLE
CCC004*    ( G40VERTRAGSNR                  CHAR(15) NOT NULL,
CCC004*      G40MANDANT                     INTEGER NOT NULL,
CCC004*      G40VBNR                        INTEGER NOT NULL,
CCC004*      G40KUNDENDATEN                 VARCHAR(322) NOT NULL,
CCC004*      G40BANK                        CHAR(20) NOT NULL,
CCC004*      G40IBAN                        CHAR(34) NOT NULL,
CCC004*      G40BIC                         CHAR(11) NOT NULL,
CCC004*      G40ANACHLASSKZ                 CHAR(1) NOT NULL,
CCC004*      G40INTVERTNR                   INTEGER NOT NULL,
CCC004*      G40VRTRGBEGINN                 CHAR(8) NOT NULL,
CCC004*      G40VERTBEGINNKZ                CHAR(1) NOT NULL,
CCC004*      G40VRTRGABLAUF                 CHAR(8) NOT NULL,
CCC004*      G40VERTABLAFFKZ                CHAR(1) NOT NULL,
CCC004*      G40KONTOART                    CHAR(2) NOT NULL,
CCC004*      G40RISIKOKLASSE                CHAR(1) NOT NULL,
CCC004*      G40VERTAENDDAT                 CHAR(8) NOT NULL,
CCC004*      G40VRTRGBEGHIST                CHAR(8) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G40_KUNDEVERTRAG            *
      ******************************************************************
       01  G40-KUNDEVERTRAG-GESAMT.
        05 NI-G40-KUNDEVERTRAG-TAB.
           10 NI-G40-KUNDEVERTRAG-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-G40-KUNDEVERTRAG REDEFINES NI-G40-KUNDEVERTRAG-TAB.
           10 NI-G40VERTRAGSNR      PIC S9(4) COMP.
           10 NI-G40MANDANT         PIC S9(4) COMP.
           10 NI-G40VBNR            PIC S9(4) COMP.
           10 NI-G40KUNDENDATEN     PIC S9(4) COMP.
           10 NI-G40BANK            PIC S9(4) COMP.
           10 NI-G40IBAN            PIC S9(4) COMP.
           10 NI-G40BIC             PIC S9(4) COMP.
           10 NI-G40ANACHLASSKZ     PIC S9(4) COMP.
           10 NI-G40INTVERTNR       PIC S9(4) COMP.
           10 NI-G40VRTRGBEGINN     PIC S9(4) COMP.
           10 NI-G40VERTBEGINNKZ    PIC S9(4) COMP.
           10 NI-G40VRTRGABLAUF     PIC S9(4) COMP.
           10 NI-G40VERTABLAFFKZ    PIC S9(4) COMP.
           10 NI-G40KONTOART        PIC S9(4) COMP.
           10 NI-G40RISIKOKLASSE    PIC S9(4) COMP.
           10 NI-G40VERTAENDDAT     PIC S9(4) COMP.
           10 NI-G40VRTRGBEGHIST    PIC S9(4) COMP.
        05 DCL-G40-KUNDEVERTRAG.
           10 G40VERTRAGSNR        PIC X(15).
           10 G40MANDANT           PIC S9(9) USAGE COMP.
           10 G40VBNR              PIC S9(9) USAGE COMP.
           10 G40KUNDENDATEN.
              49 G40KUNDENDATEN-LEN
                 PIC S9(4) USAGE COMP.
              49 G40KUNDENDATEN-TEXT
                 PIC X(322).
           10 G40BANK              PIC X(20).
           10 G40IBAN              PIC X(34).
           10 G40BIC               PIC X(11).
           10 G40ANACHLASSKZ       PIC X(1).
           10 G40INTVERTNR         PIC S9(9) USAGE COMP.
           10 G40VRTRGBEGINN       PIC X(8).
           10 G40VERTBEGINNKZ      PIC X(1).
           10 G40VRTRGABLAUF       PIC X(8).
           10 G40VERTABLAFFKZ      PIC X(1).
           10 G40KONTOART          PIC X(2).
           10 G40RISIKOKLASSE      PIC X(1).
           10 G40VERTAENDDAT       PIC X(8).
           10 G40VRTRGBEGHIST      PIC X(8).
      ************************** ENDE-DCLGEN *************************

