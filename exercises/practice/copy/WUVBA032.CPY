      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVBA032.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVBA032.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:26   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:25:02   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA032                           *
      ******************************************************************
        05  WUVBA032.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 KTO-MONAT            PIC X(10).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-KTO-STAND        PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-KTO-STAND        PIC S9(4) USAGE COMP.
           10 KTO-STAND-DISK       PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-VORSCH     PIC S999999999V99 USAGE COMP-3.
           10 KTO-STAND-RUECK      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
