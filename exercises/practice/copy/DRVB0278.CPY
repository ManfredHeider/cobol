      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0278.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0278.CPv  $
      *
      *   Rev 2.1   May 04 1999 11:41:32   T01JM2E
      *Einbau ZPKT-Kennung
      *
      *   Rev 2.0   Feb 27 1999 15:58:40   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1998 10:01:06   T13AE35
      *ARW00016 - Neu, PI/PO fuer ZTVB0072
      *
      *   Rev 1.0   23 Jul 1997 12:25:38   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0278  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    19.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !19.06.98! 01 ! NEUERSTELLUNG             ! D.WILHELM*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0278                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBVB0072 ZPKT         *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0278.
          05 PI-DRVB0278-DATEN.
             10 PI-DRVB0278-KNR-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0278-ZPKT-ZEITPUNKT    PIC  X(026).
             10 PI-DRVB0278-KTY-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0278-ZPKT-VERARB-KZ    PIC  X(001).
             10 PI-DRVB0278-ZPKT-LAST-UPDATE  PIC  X(026).
             10 PI-DRVB0278-ZPKT-KENNUNG      PIC  X(008).
      *
       01 PO-DRVB0278.
          05 PO-DRVB0278-RC                   PIC  X(003).
             88 PO-DRVB0278-RC-OK                         VALUE 'OK '.
             88 PO-DRVB0278-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRVB0278-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
