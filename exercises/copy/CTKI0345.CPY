      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0345.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/CTKI0345.CPv  $
      *
      *   Rev 2.1   Jun 10 2002 09:50:30   T13EX27
      *AKI00289 -  DB-Änderun für neue 'VP-Analyse'
      *
      *   Rev 2.0   Feb 27 1999 12:50:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   09 May 1998 13:29:36   P13SP99
      *Neuerstellung
      *
      *   Rev 1.0   Apr 18 1997 09:59:08   P13PVCS
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE                                                   *
      * 'AKI00001 V006.000 19980325'
      ******************************************************************
CCC005*    EXEC SQL DECLARE  VERSKRANKENTARIF  TABLE
CCC004*    ( KRTVERTRAGSID CHAR (13) NOT NULL,
CCC004*      KRTVERSTARLFDNR SMALLINT NOT NULL,
CCC004*      KRTPERSONENNUMMER SMALLINT,
CCC004*      KRTBEITRAGVERZ DECIMAL (3, 1),
CCC004*      KRTGESCHAEFTSSTID SMALLINT,
CCC004*      KRTGESCHAEFTSST CHAR (20),
CCC004*      KRTBEITRFRWILLWILL DECIMAL (11, 2),
CCC004*      KRTBEITRAGPRIVAT DECIMAL (11, 2)
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE                                    *
      ******************************************************************
       01  VERSKRANKENTARIF-GESAMT.
        05  NI-VERSKRANKENTARIF-TAB.
210502*    10 NI-VERSKRANKENTARIF-ELE PIC S9(04) COMP OCCURS 6 TIMES.
210502     10 NI-VERSKRANKENTARIF-ELE PIC S9(04) COMP OCCURS 8 TIMES.
        05  NI-VERSKRANKENTARIF  REDEFINES  NI-VERSKRANKENTARIF-TAB.
           10 NI-KRTVERTRAGSID            PIC S9(4) COMP.
           10 NI-KRTVERSTARLFDNR          PIC S9(4) COMP.
           10 NI-KRTPERSONENNUMMER        PIC S9(4) COMP.
           10 NI-KRTBEITRAGVERZ           PIC S9(4) COMP.
           10 NI-KRTGESCHAEFTSSTID        PIC S9(4) COMP.
           10 NI-KRTGESCHAEFTSST          PIC S9(4) COMP.
210502     10 NI-KRTBEITRFRWILLWILL       PIC S9(4) COMP.
210502     10 NI-KRTBEITRAGPRIVAT         PIC S9(4) COMP.
        05  DCL-VERSKRANKENTARIF.
           10 KRTVERTRAGSID               PIC X(13).
           10 KRTVERSTARLFDNR             PIC S9(4) COMP.
           10 KRTPERSONENNUMMER           PIC S9(4) COMP.
           10 KRTBEITRAGVERZ              PIC S9(2)V9(1) COMP-3.
           10 KRTGESCHAEFTSSTID           PIC S9(4) COMP.
           10 KRTGESCHAEFTSST             PIC X(20).
210502     10 KRTBEITRFRWILLWILL          PIC S9(9)V9(2) COMP-3.
210502     10 KRTBEITRAGPRIVAT            PIC S9(9)V9(2) COMP-3.
      ******************************************************************
