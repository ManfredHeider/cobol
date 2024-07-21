      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0283.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0283.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:42   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1998 10:01:40   T13AE35
      *ARW00016 - Neu, PI/PO fuer ZTVB0072
      *
      *   Rev 1.0   23 Jul 1997 12:26:14   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0283  FUNKTION SELECT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: D.WILHELM                                       *
      *    DATUM:    22.06.1998                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      *         !22.06.98! 01 ! NEUERSTELLUNG             ! D.WILHELM*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0283                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  SELECT  <<<       TABELLE      TBVB0072 ZPKT         *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0283.
          05 PI-DRVB0283-DATEN.
             10 PI-DRVB0283-KNR-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0283-ZPKT-ZEITPUNKT    PIC  X(026).
      *
       01 PO-DRVB0283.
          05 PO-DRVB0283-DATEN.
             10 PO-DRVB0283-KTY-STEUER-INFO   PIC S9(004) BINARY.
             10 PO-DRVB0283-ZPKT-VERARB-KZ    PIC  X(001).
             10 PO-DRVB0283-ZPKT-LAST-UPDATE  PIC  X(026).
             10 PO-DRVB0283-ZPKT-KENNUNG      PIC  X(026).
          05 PO-DRVB0283-RC                   PIC  X(003).
             88 PO-DRVB0283-RC-OK                         VALUE 'OK '.
             88 PO-DRVB0283-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRVB0283-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      ****** Ende Copy-Member ****************************************
