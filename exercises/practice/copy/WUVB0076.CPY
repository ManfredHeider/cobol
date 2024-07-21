      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0076.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0076.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:22   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0076                           *
      ******************************************************************
        05  WUVB0076.
           10 OBAZ-ST-NR           PIC S9(9) USAGE COMP.
           10 OBAZ-ANSPR-VOM       PIC X(10).
           10 OBAZ-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4) USAGE COMP.
           10 OBAZ-ANSPR-BETRAG    PIC S999999999V99 USAGE COMP-3.
           10 KTY-ANSPR-STATUS     PIC S9(4) USAGE COMP.
           10 KNR-ANSPR-STATUS     PIC S9(4) USAGE COMP.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 8       *
      ******************************************************************

