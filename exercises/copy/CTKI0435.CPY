      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CTKI0435.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/CTKI0435.CPv  $
      *
      *   Rev 2.3   Mar 02 2015 16:24:22   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.2   Sep 01 2014 10:36:38   t13ex27
      *AKI01315: Masch. Verarbeitung von Fonds- und Paletteninforma
      *tionen-DB-Änderung und OZPs
      *
      *   Rev 2.1   04 Jun 2008 15:59:34   t13ex27
      *AKI00711: Neue DTA-Felder AMLeben DB-Änderung
      *
      *   Rev 2.0   07 May 2008 09:39:10   t13ex27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * DCLGEN TABLE FONDSPALETTE
      ******************************************************************
CCC005*    EXEC SQL DECLARE FONDSPALETTE TABLE
CCC004*    ( FOPPALETTENNAME                CHAR(8) NOT NULL,
CCC004*      FOPFONDSKUERZEL                CHAR(2) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE FONDSPALETTE                *
      ******************************************************************
       01  FONDSPALETTE-GESAMT.
        05 NI-FONDSPALETTE-TAB.
           10 NI-FONDSPALETTE-ELE PIC S9(04) COMP OCCURS 2 TIMES.
        05 NI-FONDSPALETTE REDEFINES NI-FONDSPALETTE-TAB.
           10 NI-FOPPALETTENNAME    PIC S9(4) COMP.
           10 NI-FOPFONDSKUERZEL    PIC S9(4) COMP.
        05 DCL-FONDSPALETTE.
           10 FOPPALETTENNAME      PIC X(8).
           10 FOPFONDSKUERZEL      PIC X(2).
      ************************** ENDE-DCLGEN *************************

