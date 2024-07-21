      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0058.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0058.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:08   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0058                           *
      ******************************************************************
        05  WUVB0058.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 PSEV-MONAT           PIC X(10).
           10 KNR-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-PROD-SPARTE      PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 PSEV-EH-GS-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-GS-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-PV-GS-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-GS-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-FG-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-PV-FG-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 PSEV-EH-FG-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 PSEV-EH-FG-AB-GR     PIC S999999999V9 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 18      *
      ******************************************************************
