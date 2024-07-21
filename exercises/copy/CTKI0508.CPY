      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30/08/18
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0508.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTKI0508.CPv  $
      *
      *   Rev 2.1   Oct 22 2018 11:49:14   t13ex39
      *AKI01615: Generali Invest  OZPs  WHO   
      *
      *   Rev 2.0   Aug 30 2018 15:27:54   T13EX39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE G42_ZUS_KUNDEN TABLE
CCC004*    ( G42ZKVERTRAGSNR                CHAR(15) NOT NULL,
CCC004*      G42ZKLFDNR                     INTEGER NOT NULL,
CCC004*      G42ZKNACHNAME                  CHAR(35) NOT NULL,
CCC004*      G42ZKVORNAME                   CHAR(35) NOT NULL,
CCC004*      G42ZKGEBDAT                    CHAR(8) NOT NULL,
CCC004*      G42ZKGESCHLECHT                CHAR(1) NOT NULL,
CCC004*      G42ZKANREDE                    CHAR(1) NOT NULL,
CCC004*      G42ZKSTRHNR                    CHAR(40) NOT NULL,
CCC004*      G42ZKPLZ                       CHAR(9) NOT NULL,
CCC004*      G42ZKORT                       CHAR(40) NOT NULL,
CCC004*      G42ZKLKZ                       CHAR(3) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE G42_ZUS_KUNDEN              *
      ******************************************************************
       01  G42-ZUS-KUNDEN-GESAMT.
        05 NI-G42-ZUS-KUNDEN-TAB.
           10 NI-G42-ZUS-KUNDEN-ELE PIC S9(04) COMP OCCURS 11 TIMES.
        05 NI-G42-ZUS-KUNDEN REDEFINES NI-G42-ZUS-KUNDEN-TAB.
           10 NI-G42ZKVERTRAGSNR    PIC S9(4) COMP.
           10 NI-G42ZKLFDNR         PIC S9(4) COMP.
           10 NI-G42ZKNACHNAME      PIC S9(4) COMP.
           10 NI-G42ZKVORNAME       PIC S9(4) COMP.
           10 NI-G42ZKGEBDAT        PIC S9(4) COMP.
           10 NI-G42ZKGESCHLECHT    PIC S9(4) COMP.
           10 NI-G42ZKANREDE        PIC S9(4) COMP.
           10 NI-G42ZKSTRHNR        PIC S9(4) COMP.
           10 NI-G42ZKPLZ           PIC S9(4) COMP.
           10 NI-G42ZKORT           PIC S9(4) COMP.
           10 NI-G42ZKLKZ           PIC S9(4) COMP.
        05 DCL-G42_ZUS_KUNDEN.
           10 G42ZKVERTRAGSNR      PIC X(15).
           10 G42ZKLFDNR           PIC S9(9) USAGE COMP.
           10 G42ZKNACHNAME        PIC X(35).
           10 G42ZKVORNAME         PIC X(35).
           10 G42ZKGEBDAT          PIC X(8).
           10 G42ZKGESCHLECHT      PIC X(1).
           10 G42ZKANREDE          PIC X(1).
           10 G42ZKSTRHNR          PIC X(40).
           10 G42ZKPLZ             PIC X(9).
           10 G42ZKORT             PIC X(40).
           10 G42ZKLKZ             PIC X(3).
      ************************** ENDE-DCLGEN *************************

