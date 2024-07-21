      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTOK0009.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CTOK0009.CPv  $
      *
      *   Rev 2.1   19 Jan 2007 09:50:32   t13ex43
      *AKI00556: Steuersätze für Kundenzusammenführung
      *
      *   Rev 2.0   Mar 14 2006 16:40:38   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE OKITICKETWERTE
      ******************************************************************
CCC005*    EXEC SQL DECLARE OKITICKETWERTE TABLE
CCC004*    ( OTWTICKETID                    DECIMAL(18, 0) NOT NULL,
CCC004*      OTWSCHLUESSEL                  CHAR(30) NOT NULL,
CCC004*      OTWWERTALT                     VARCHAR(256) NOT NULL,
CCC004*      OTWWERTNEU                     VARCHAR(256) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  OKITICKETWERTE
      ******************************************************************
       01  OKITICKETWERTE-GESAMT.
         05 NI-OKITICKETWERTE-TAB.
           10 NI-OKITICKETWERTE-ELE PIC S9(4) COMP OCCURS 4 TIMES.
         05 NI-OKITICKETWERTE REDEFINES NI-OKITICKETWERTE-TAB.
           10 NI-OTWTICKETID        PIC S9(4) COMP.
           10 NI-OTWSCHLUESSEL      PIC S9(4) COMP.
           10 NI-OTWWERTALT         PIC S9(4) COMP.
           10 NI-OTWWERTNEU         PIC S9(4) COMP.
         05 DCL-OKITICKETWERTE.
           10 OTWTICKETID          PIC S9(18)V COMP-3.
           10 OTWSCHLUESSEL        PIC X(30).
           10 OTWWERTALT.
              49 OTWWERTALT-LEN    PIC S9(4) COMP.
              49 OTWWERTALT-TEXT   PIC X(256).
           10 OTWWERTNEU.
              49 OTWWERTNEU-LEN    PIC S9(4) COMP.
              49 OTWWERTNEU-TEXT   PIC X(256).
      ******************************************************************