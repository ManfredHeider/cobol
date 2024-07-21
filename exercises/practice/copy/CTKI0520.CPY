      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18/12/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0520.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0520.CPv  $
      *
      *   Rev 2.1   Jan 30 2019 11:36:38   t13ex39
      *AKI01652: DeuBa TT für Zusätzliche Kontoinhaber
      *
      *   Rev 2.0   Dec 18 2018 06:22:14   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G48_ZUS_KUNDEN TABLE
CCC004*    ( G48ZKVERTRAGSNR                CHAR(15) NOT NULL,
CCC004*      G48ZKLFDNR                     SMALLINT NOT NULL,
CCC004*      G48ZKNACHNAME                  CHAR(35) NOT NULL,
CCC004*      G48ZKVORNAME                   CHAR(35) NOT NULL,
CCC004*      G48ZKGEBDAT                    CHAR(8) NOT NULL,
CCC004*      G48ZKGESCHLECHT                CHAR(1) NOT NULL,
CCC004*      G48ZKANREDE                    CHAR(1) NOT NULL,
CCC004*      G48ZKSTR                       CHAR(28) NOT NULL,
CCC004*      G48ZKHAUSNR                    CHAR(9) NOT NULL,
CCC004*      G48ZKPOSTFACH                  CHAR(9) NOT NULL,
CCC004*      G48ZKPLZ                       CHAR(9) NOT NULL,
CCC004*      G48ZKORT                       CHAR(28) NOT NULL,
CCC004*      G48ZKLKZ                       CHAR(3) NOT NULL,
CCC004*      G48ZKTELVOR                    CHAR(6) NOT NULL,
CCC004*      G48ZKTELNR                     CHAR(9) NOT NULL,
CCC004*      G48ZKDEPOTKZ                   CHAR(1) NOT NULL,
CCC004*      G48ZKDELKZ                     CHAR(1) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G48_ZUS_KUNDEN              *
      ******************************************************************
       01  G48-ZUS-KUNDEN-GESAMT.
        05 NI-G48-ZUS-KUNDEN-TAB.
           10 NI-G48-ZUS-KUNDEN-ELE PIC S9(04) COMP OCCURS 17 TIMES.
        05 NI-G48-ZUS-KUNDEN REDEFINES NI-G48-ZUS-KUNDEN-TAB.
           10 NI-G48ZKVERTRAGSNR    PIC S9(4) COMP.
           10 NI-G48ZKLFDNR         PIC S9(4) COMP.
           10 NI-G48ZKNACHNAME      PIC S9(4) COMP.
           10 NI-G48ZKVORNAME       PIC S9(4) COMP.
           10 NI-G48ZKGEBDAT        PIC S9(4) COMP.
           10 NI-G48ZKGESCHLECHT    PIC S9(4) COMP.
           10 NI-G48ZKANREDE        PIC S9(4) COMP.
           10 NI-G48ZKSTR           PIC S9(4) COMP.
           10 NI-G48ZKHAUSNR        PIC S9(4) COMP.
           10 NI-G48ZKPOSTFACH      PIC S9(4) COMP.
           10 NI-G48ZKPLZ           PIC S9(4) COMP.
           10 NI-G48ZKORT           PIC S9(4) COMP.
           10 NI-G48ZKLKZ           PIC S9(4) COMP.
           10 NI-G48ZKTELVOR        PIC S9(4) COMP.
           10 NI-G48ZKTELNR         PIC S9(4) COMP.
           10 NI-G48ZKDEPOTKZ       PIC S9(4) COMP.
           10 NI-G48ZKDELKZ         PIC S9(4) COMP.
        05 DCL-G48_ZUS_KUNDEN.
           10 G48ZKVERTRAGSNR      PIC X(15).
           10 G48ZKLFDNR           PIC S9(4) USAGE COMP.
           10 G48ZKNACHNAME        PIC X(35).
           10 G48ZKVORNAME         PIC X(35).
           10 G48ZKGEBDAT          PIC X(8).
           10 G48ZKGESCHLECHT      PIC X(1).
           10 G48ZKANREDE          PIC X(1).
           10 G48ZKSTR             PIC X(28).
           10 G48ZKHAUSNR          PIC X(9).
           10 G48ZKPOSTFACH        PIC X(9).
           10 G48ZKPLZ             PIC X(9).
           10 G48ZKORT             PIC X(28).
           10 G48ZKLKZ             PIC X(3).
           10 G48ZKTELVOR          PIC X(6).
           10 G48ZKTELNR           PIC X(9).
           10 G48ZKDEPOTKZ         PIC X(1).
           10 G48ZKDELKZ           PIC X(1).
      ************************** ENDE-DCLGEN *************************

