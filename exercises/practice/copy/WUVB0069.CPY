      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WUVB0069.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\WUVB0069.cpv  $
      *
      *   Rev 2.1   12 Apr 2001 19:44:18   T13EX31
      *AVB00525 - EURO-Umstellung: DB2-Tabellenstrukturen
      *
      *   Rev 2.0   02 Mar 2001 08:24:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBVB0069                           *
      ******************************************************************
        05  WUVB0069.
           10 ST-NR                PIC S9(9)    USAGE COMP.
           10 SDLB-MONAT           PIC X(10).
           10 SDLB-DARL-NR         PIC S9(4)    USAGE COMP.
           10 KNR-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SDLB-LAST-UPDATE     PIC X(26).
           10 KTY-WAEHRUNG         PIC S9(4)    USAGE COMP.
           10 SDLB-RATE-EINBH      PIC S9(9)V99 USAGE COMP-3.
200596*    10 SDLB-ZAHLUNG-MA      PIC S9(9)V99 USAGE COMP-3.
           10 SDLB-ZINSEN          PIC S9(9)V99 USAGE COMP-3.
           10 SDLB-DARL-BETRAG     PIC S9(9)V99 USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 9       *
      ******************************************************************
