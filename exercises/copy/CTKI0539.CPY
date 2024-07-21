      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14/04/20
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0539.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0539.CPv  $
      *
      *   Rev 2.3   Oct 06 2020 16:39:02   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.2   Oct 06 2020 11:50:34   u130684
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.1   Jun 23 2020 12:03:20   t13ex27
      *AKI01773: Anbindung Compeon an DV - OZPs
      *
      *   Rev 2.0   Apr 14 2020 15:14:45   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE DVMCOMPEON TABLE
CCC004*    ( DCOVERTRAGSID                  CHAR(13) NOT NULL,
CCC004*      DCOPRODUKTID                   SMALLINT NOT NULL,
CCC004*      DCOPRODUKTART                  CHAR(1) NOT NULL,
CCC004*      DCOFINBETRAG                   DECIMAL(14, 2) NOT NULL,
CCC004*      DCOZUSAGE                      DATE,
CCC004*      DCOAUSZAHLUNG                  DATE,
CCC004*      DCOPRODUKTGEBER                CHAR(30),
CCC004*      DCOAKTSALDO                    DECIMAL(14, 2),
CCC004*      DCOSOLLZINS                    DECIMAL(6, 4) NOT NULL,
CCC004*      DCORATE                        DECIMAL(14, 2) NOT NULL,
CCC004*      DCOFAELLIG                     DATE,
CCC004*      DCORESTLAUF                    DECIMAL(3, 0) NOT NULL,
CCC004*      DCOIBANKREDIT                  CHAR(34),
CCC004*      DCODELETEKZ                    CHAR(1) NOT NULL,
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE DVMCOMPEON                  *
      ******************************************************************
       01  DVMCOMPEON-GESAMT.
        05 NI-DVMCOMPEON-TAB.
           10 NI-DVMCOMPEON-ELE PIC S9(04) COMP OCCURS 15 TIMES.
        05 NI-DVMCOMPEON REDEFINES NI-DVMCOMPEON-TAB.
           10 NI-DCOVERTRAGSID      PIC S9(4) COMP.
           10 NI-DCOPRODUKTID       PIC S9(4) COMP.
           10 NI-DCOPRODUKTART      PIC S9(4) COMP.
           10 NI-DCOFINBETRAG       PIC S9(4) COMP.
           10 NI-DCOZUSAGE          PIC S9(4) COMP.
           10 NI-DCOAUSZAHLUNG      PIC S9(4) COMP.
           10 NI-DCOPRODUKTGEBER    PIC S9(4) COMP.
           10 NI-DCOAKTSALDO        PIC S9(4) COMP.
           10 NI-DCOSOLLZINS        PIC S9(4) COMP.
           10 NI-DCORATE            PIC S9(4) COMP.
           10 NI-DCOFAELLIG         PIC S9(4) COMP.
           10 NI-DCORESTLAUF        PIC S9(4) COMP.
           10 NI-DCOIBANKREDIT      PIC S9(4) COMP.
           10 NI-DCODELETEKZ        PIC S9(4) COMP.
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.
        05 DCL-DVMCOMPEON.
           10 DCOVERTRAGSID        PIC X(13).
           10 DCOPRODUKTID         PIC S9(4) USAGE COMP.
           10 DCOPRODUKTART        PIC X(1).
           10 DCOFINBETRAG         PIC S9(12)V9(2) USAGE COMP-3.
           10 DCOZUSAGE            PIC X(10).
           10 DCOAUSZAHLUNG        PIC X(10).
           10 DCOPRODUKTGEBER      PIC X(30).
           10 DCOAKTSALDO          PIC S9(12)V9(2) USAGE COMP-3.
           10 DCOSOLLZINS          PIC S9(2)V9(4) USAGE COMP-3.
           10 DCORATE              PIC S9(12)V9(2) USAGE COMP-3.
           10 DCOFAELLIG           PIC X(10).
           10 DCORESTLAUF          PIC S9(3)V USAGE COMP-3.
           10 DCOIBANKREDIT        PIC X(34).
           10 DCODELETEKZ          PIC X(1).
           10 LETZTEAENDERUNG      PIC X(26).
      ************************** ENDE-DCLGEN *************************


