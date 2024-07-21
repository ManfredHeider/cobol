      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3027.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3027.CPv  $
      *
      *   Rev 2.1   Jul 09 2015 09:42:06   t13ex48
      *AKI01372: AML: Einführung Teil- und Zeitrenten, Kapitalentna
      *hmen: Ermittlung, Aufbereitung
      *
      *   Rev 2.0   Jul 02 2015 08:23:18   t13ex48
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *              #####################                             *
      *     ########    D R K I 3 0 2 7    ########                    *
      *              #####################                             *
      *                                                                *
      *  COPY-MEMBER FUER AUSGABE AUFBEREITUNG AML GELEISTETE RENTEN   *
      *                                                                *
      *    SATZLÄNGE   100                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      ******************************************************************
      * AENDERUNGEN:                                                   *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME       *
      *---------!--------!----!---------------------------!------------*
      *AKI01372 !01.07.15!    !NEUERSTELLUNG              ! HSE        *
      *         !        !    !                           !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
       01  AML-GEL-RENTE-AUFB.
           05  AMLGR-A-KEY.
               10  AMLGR-A-GES                     PIC 9(02).
               10  AMLGR-A-VNR                     PIC 9(15).
               10  AMLGR-A-RNTBEGINN               PIC X(10).
               10  AMLGR-A-RNTART                  PIC S9(04).
      *  Kann die Werte 1 und 3 annehmen
           05  AMLGR-A-DATEN.
               10  AMLGR-A-ABLAUF                  PIC X(10).
      *  Ablaufdatum: ist evtl mit Initialwert 31.12.9999 belegt
               10  AMLGR-A-ZAHLW-KTY               PIC S9(04).
      *  TBAT0038: KTY = 8
               10  AMLGR-A-ZAHLW-KNR               PIC S9(04).
      *  Beim VEP immer monatlich , d.h. 6 (AML liefert 12)
               10  AMLGR-A-GSMTHOEHE               PIC S9(11)V99.
      *  Höhe der Teil- oder Zeitrente pro Monat
           05  AMLGR-A-AKTUAL-KZ                   PIC X(01).
           05  AMLGR-A-RESERVE                     PIC X(37).

