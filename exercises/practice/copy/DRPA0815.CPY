      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0815.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0815.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:23:04   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:43:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drpa0815  Funktion Select Standard-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: opritz                                          *
      *    DATUM:    22.12.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!28.01.98! 01 ! NEUERSTELLUNG             ! opritz   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPa0815                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>schreiben<<<    schreiben ROOT DATEI                   *
      *                                                              *
      ****************************************************************
       01 PI-DRPa0815.
          05 PI-DRPa0815-ROOT-DATEI            PIC X(8).
          05 pi-DRPa0815-ROOT-DATEN.
             10 pi-DRPa0815-P1SA                PIC XX.
             10 pi-DRPa0815-root-key.
                15 pi-DRPa0815-P1GES            PIC XX.
                15 pi-DRPa0815-P1VSNR           PIC 9(15).
             10 pi-DRPa0815-P1INTVSNR           PIC 9(15).
             10 pi-DRPa0815-P1SATZ.
                15 pi-DRPa0815-P1EKRIT.
                    20 pi-DRPa0815-P1EPGM       PIC X(8).
                    20 pi-DRPa0815-P1EDATUM     PIC 9(7)  COMP-3.
                    20 pi-DRPa0815-P1EZEIT      PIC 9(7)  COMP-3.
                    20 pi-DRPa0815-P1PSNR       PIC 9(7)  COMP-3.
                15 pi-DRPa0815-P1AVERM          PIC 9(7)  COMP-3.
                15 pi-DRPa0815-P1AVERMPRZ       PIC X.
                15 pi-DRPa0815-P1VVERM          PIC 9(7)  COMP-3.
                15 pi-DRPa0815-P1VVERMPRZ       PIC X.
                15 pi-DRPa0815-P1KDBETRGV       PIC 9(7)  COMP-3.
                15 pi-DRPa0815-P1NAME           PIC X(14).
                15 pi-DRPa0815-P1VSBEGINN       PIC 9(7)  COMP-3.
                15 pi-DRPa0815-P1ZAHLWEISE      PIC 9.
                15 pi-DRPa0815-P1DDRKZ          PIC X(1).
                15 pi-DRPa0815-P1FILLER         PIC X(2).
                15 pi-DRPa0815-P1KDBETRANZ      PIC 99.
                15 pi-DRPa0815-P1TABBETR
                       OCCURS 1 TO 12 DEPENDING ON
                                pi-DRPa0815-P1KDBETRANZ.
                    20 pi-DRPa0815-P1KDBETR     PIC 9(7)  COMP-3.
                    20 pi-DRPa0815-P1GUELTAB    PIC 9(7)  COMP-3.
                    20 pi-DRPa0815-P1AENDDAT    PIC 9(7)  COMP-3.
      *
       01 PO-DRPa0815.
          05 PO-DRPa0815-RC                    PIC  X(003).
             88 PO-DRPa0815-RC-OK                       VALUE 'OK '.
             88 PO-DRPa0815-RC-DUPKEY                   VALUE 'DVB'.
             88 PO-DRPa0815-RC-NOT-FOUND                VALUE 'NF '.


