      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVBA033.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVBA033.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:28   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:25:04   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVBA033                           *
      ******************************************************************
        05  WUVBA033.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 EV-MONAT             PIC X(10).
           10 KNR-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-VERGUET-TYP      PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 EV-EQF               PIC S999V99 USAGE COMP-3.
           10 EV-STORMIND-PROZ     PIC S999V99 USAGE COMP-3.
           10 EV-AE-MONATS-DS      PIC S999999999V9 USAGE COMP-3.
           10 EV-BRUTTO-BETRAG     PIC S999999999V99 USAGE COMP-3.
200197     10 EV-RUECKBELASTG      PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
