      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0026.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0026.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:08   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
030295******************************************************************
030295* COBOL DECLARATION FOR TABLE VIVB0026                           *
030295******************************************************************
030295  05  WUVB0026.
030295     10 MA-ST-NR             PIC S9(9) USAGE COMP.
030295     10 OLZZ-MONAT           PIC X(10).
030295     10 KNR-OLZ-TYP          PIC S9(4) USAGE COMP.
030295     10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
030295     10 OLZZ-LAST-UPDATE     PIC X(26).
030295     10 KTY-OLZ-TYP          PIC S9(4) USAGE COMP.
030295     10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
030295     10 OLZZ-BETRAG          PIC S999999999V99 USAGE COMP-3.
030295******************************************************************
030295* THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
030295******************************************************************
