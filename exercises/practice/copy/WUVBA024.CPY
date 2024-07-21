      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVBA024.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVBA024.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:24   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:25:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVBA024                           *
      ******************************************************************
        05  WUVBA024.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 VSWZ-LAST-UPDATE     PIC X(26).
           10 VSWZ-ZUSAGE-DAT      PIC X(10).
           10 VSWZ-A-IG-DAT        PIC X(10).
           10 NIL-VSWZ-A-IG-DAT    PIC X(01).
           10 VSWZ-A-GESAMT        PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-G-DAT-VON       PIC X(10).
           10 NIL-VSWZ-G-DAT-VON   PIC X(01).
           10 VSWZ-G-STZ-JAHR      PIC X(10).
           10 NIL-VSWZ-G-STZ-JAHR  PIC X(01).
           10 VSWZ-G-VERS-SUM      PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-G-STZ-ANZ       PIC S9(4) USAGE COMP.
           10 VSWZ-G-VERS-NR       PIC S999999999999999V USAGE COMP-3.
           10 VSWZ-Z-VERS-SUM      PIC S999999999V99 USAGE COMP-3.
           10 VSWZ-Z-VERS-NR       PIC S999999999999999V USAGE COMP-3.
           10 VSWZ-Z-DAT-VON       PIC X(10).
           10 NIL-VSWZ-Z-DAT-VON   PIC X(01).
           10 KTY-A-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KNR-A-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KTY-G-BERECHTIG      PIC S9(4) USAGE COMP.
           10 KNR-G-BERECHTIG      PIC S9(4) USAGE COMP.
           10 VSWZ-DBI-DAT         PIC X(10).
           10 NIL-VSWZ-DBI-DAT     PIC X(01).
           10 VSWZ-DBI-KTO-NR      PIC X(9).
           10 NIL-VSWZ-DBI-KTO-NR  PIC X(01).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
