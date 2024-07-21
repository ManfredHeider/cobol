      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0063.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0063.CPv  $
      *
      *   Rev 2.1   12 Apr 2001 19:43:44   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:50   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0063                           *
      ******************************************************************
        05  WUVB0063.
           10 ST-NR                PIC S9(9) USAGE COMP.
           10 BER-HAFTUNG          PIC X(1).
           10 BER-L-PROV-BEW       PIC X(1).
           10 BER-STAND-DISK       PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-RUECK      PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-VORSCH     PIC S999999999V99 USAGE COMP-3.
           10 BER-STAND-ABSCHR     PIC S999999999V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************
