      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0066.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0066.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:14   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0066                           *
      ******************************************************************
        05  WUVB0066.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SPFS-PFND-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SPFS-LAST-UPDATE     PIC X(26).
           10 KNR-PFND-ART         PIC S9(4)    USAGE COMP.
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 KTY-PFND-ART         PIC S9(4)    USAGE COMP.
           10 SPFS-PFND-RANG       PIC S9(4)    USAGE COMP.
200596     10 KTY-PFAND-STATUS     PIC S9(4)    USAGE COMP.
200596     10 KNR-PFAND-STATUS     PIC S9(4)    USAGE COMP.
           10 SPFS-FORD-GS         PIC S9(9)V99 USAGE COMP-3.
           10 SPFS-RATE-UNTERH     PIC S9(9)V99 USAGE COMP-3.
           10 SPFS-RATE-PFND-V     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 13      *
      ******************************************************************
