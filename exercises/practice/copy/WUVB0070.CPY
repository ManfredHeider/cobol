      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0070.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0070.CPv  $
      *
      *   Rev 2.1   12 Apr 2001 19:43:46   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0070                           *
      ******************************************************************
        05  WUVB0070.
           10 KIRH-ALTER           PIC S9(4)    USAGE COMP.
           10 KNR-GESCHL           PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-LAST-UPDATE     PIC X(26).
           10 KTY-HESCHL           PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KIRH-VERS-SUM        PIC S9(9)V99 USAGE COMP-3.
           10 KIRH-ABL-SUM         PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************
