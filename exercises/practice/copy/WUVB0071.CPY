      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0071.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0071.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:20   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0071                           *
      ******************************************************************
        05  WUVB0071.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 KNR-KTO-TYP          PIC S9(4)    USAGE COMP.
           10 KTOB-BUCH-DAT        PIC X(10).
           10 KTOB-LAST-UPDATE     PIC X(26).
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTY-KTO-TYP          PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTOB-BUCH-TEXT       PIC X(40).
           10 KTOB-BUCH-BETR       PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
