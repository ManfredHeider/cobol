      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA0814.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRPA0814.CPv  $
      *
      *   Rev 2.0   May 26 1999 17:23:02   T13EX21
      *APA00012 - Neuerstellung
      *
      *   Rev 1.0   14 Jan 1998 12:43:52   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: drpa0814  Funktion Select Standard-OZP       *
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
      *    FÜR DIE FUNKTION DRPA0814                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  LESEN  <<<        LESEN ROOT DATEI                   *
      *                                                              *
      ****************************************************************
       01 PI-DRPA0814.
          05 PI-DRPA0814-UFKT                  PIC X(01).
             88 PI-DRPA0814-UFKT-VNR                    VALUE '1'.
             88 PI-DRPA0814-UFKT-PIN                    VALUE '2'.
          05 PI-DRPA0814-ROOT-KEY-1.
              10 PI-DRPA0814-P1GES             PIC 99.
              10 PI-DRPA0814-P1VSNR            PIC 9(15).
          05 PI-DRPA0814-ROOT-KEY-2.
              10 PI-DRPA0814-P1INTVSNR         PIC 9(15).
      *
       01 PO-DRPA0814.
          05 PO-DRPA0814-RC                    PIC  X(003).
             88 PO-DRPA0814-RC-OK                       VALUE 'OK '.
             88 PO-DRPA0814-RC-NOT-FOUND                VALUE 'NFD'.
          05 PO-DRPA0814-ROOT-DATEN.
             10 PO-DRPA0814-P1SA                PIC XX.
             10 PO-DRPA0814-P1KEY.
                15 PO-DRPA0814-P1GES            PIC XX.
                15 PO-DRPA0814-P1VSNR           PIC 9(15).
             10 PO-DRPA0814-P1INTVSNR           PIC 9(15).
             10 PO-DRPA0814-P1SATZ.
                15 PO-DRPA0814-P1EKRIT.
                    20 PO-DRPA0814-P1EPGM       PIC X(8).
                    20 PO-DRPA0814-P1EDATUM     PIC 9(7)  COMP-3.
                    20 PO-DRPA0814-P1EZEIT      PIC 9(7)  COMP-3.
                    20 PO-DRPA0814-P1PSNR       PIC 9(7)  COMP-3.
                15 PO-DRPA0814-P1AVERM          PIC 9(7)  COMP-3.
                15 PO-DRPA0814-P1AVERMPRZ       PIC X.
                15 PO-DRPA0814-P1VVERM          PIC 9(7)  COMP-3.
                15 PO-DRPA0814-P1VVERMPRZ       PIC X.
                15 PO-DRPA0814-P1KDBETRGV       PIC 9(7)  COMP-3.
                15 PO-DRPA0814-P1NAME           PIC X(14).
                15 PO-DRPA0814-P1VSBEGINN       PIC 9(7)  COMP-3.
                15 PO-DRPA0814-P1ZAHLWEISE      PIC 9.
                15 PO-DRPA0814-P1DDRKZ          PIC X(1).
                15 PO-DRPA0814-P1FILLER         PIC X(2).
                15 PO-DRPA0814-P1KDBETRANZ      PIC 99.
                15 PO-DRPA0814-P1TABBETR
                       OCCURS 1 TO 12 DEPENDING ON
                                PO-DRPA0814-P1KDBETRANZ.
                    20 PO-DRPA0814-P1KDBETR     PIC 9(7)  COMP-3.
                    20 PO-DRPA0814-P1GUELTAB    PIC 9(7)  COMP-3.
                    20 PO-DRPA0814-P1AENDDAT    PIC 9(7)  COMP-3.


