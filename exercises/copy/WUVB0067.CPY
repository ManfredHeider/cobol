      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0067.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0067.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:16   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0067                           *
      ******************************************************************
        05  WUVB0067.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SDLS-DARL-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SDLS-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
200596     10 KTY-DARL-STATUS      PIC S9(4)    USAGE COMP.
200596     10 KNR-DARL-STATUS      PIC S9(4)    USAGE COMP.
           10 SDLS-DARL-BETRAG     PIC S9(9)V99 USAGE COMP-3.
           10 SDLS-AUSZAHL-DAT     PIC X(10).
           10 SDLS-RATE-V          PIC S9(9)V99 USAGE COMP-3.
           10 SDLS-RATE-V-UEBD     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
