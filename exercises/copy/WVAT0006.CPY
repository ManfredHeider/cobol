      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVAT0006.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\WVAT0006.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 11:57:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   Nov 04 1995 10:11:44   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIAT0006                           *
      ******************************************************************
         05 WB-NI-VIAT0006-TAB.
           10 WB-NI-VIAT0006-ELE      PIC S9(4) USAGE COMP  OCCURS  2.
         05 WB-NI-VIAT0006            REDEFINES WB-NI-VIAT0006-TAB.
           10 WB-NI-DUMMY-DATE        PIC S9(4) USAGE COMP.
           10 WB-NI-CICS-DAT          PIC S9(4) USAGE COMP.
         05  WB-VIAT0006.
           10 DUMMY-DATE              PIC X(10).
           10 WB-CICS-DAT             PIC X(10).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 1       *
      ******************************************************************
