      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0065.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0065.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:12   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:52   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR VIEW  VIVB0065                           *
      ******************************************************************
        05  WUVB0065.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 GEEV-MONAT           PIC X(10).
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 GEEV-EH-GS-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-GS-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-PV-GS-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-GS-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-FG-NT-EG     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-PV-FG-NT-GR     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-EH-FG-ZU-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-ZU-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-AB-EG     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-EH-FG-AB-GR     PIC S999999999V9 USAGE COMP-3.
           10 GEEV-PRV-RCK-ZFL     PIC S999999999V99 USAGE COMP-3.
           10 GEEV-RCK-AUSZ-Q      PIC S999999999V99 USAGE COMP-3.
           10 GEEV-UEBWEIS         PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
