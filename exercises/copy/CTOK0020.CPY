      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11/09/12
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0020.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0020.CPv  $
      *
      *   Rev 2.1   Oct 08 2012 14:02:40   t13ex27
      *AOK00047: Auswertung Einarbeitungszeiten und Satzstatistik-S
      *atzstatistik
      *
      *   Rev 2.0   Sep 11 2012 17:30:33   T13EX27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
CCC005*    EXEC SQL DECLARE OKITABSTATISTIK TABLE
CCC004*    ( OTSDATUM                       DATE NOT NULL,
CCC004*      OTSTABELLE                     CHAR(30) NOT NULL,
CCC004*      OTSINSERT                      INTEGER NOT NULL,
CCC004*      OTSUPDATE                      INTEGER NOT NULL,
CCC004*      OTSDELETE                      INTEGER NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE OKITABSTATISTIK             *
      ******************************************************************
       01  OKITABSTATISTIK-GESAMT.
        05 NI-OKITABSTATISTIK-TAB.
           10 NI-OKITABSTATISTIK-ELE PIC S9(04) COMP OCCURS 5 TIMES.
        05 NI-OKITABSTATISTIK REDEFINES NI-OKITABSTATISTIK-TAB.
           10 NI-OTSDATUM           PIC S9(4) COMP.
           10 NI-OTSTABELLE         PIC S9(4) COMP.
           10 NI-OTSINSERT          PIC S9(4) COMP.
           10 NI-OTSUPDATE          PIC S9(4) COMP.
           10 NI-OTSDELETE          PIC S9(4) COMP.
        05 DCL-OKITABSTATISTIK.
           10 OTSDATUM             PIC X(10).
           10 OTSTABELLE           PIC X(30).
           10 OTSINSERT            PIC S9(9) USAGE COMP.
           10 OTSUPDATE            PIC S9(9) USAGE COMP.
           10 OTSDELETE            PIC S9(9) USAGE COMP.
      ************************** ENDE-DCLGEN *************************
