      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0319.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0319.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Nov 1998 10:02:10   T13AE35
      *ARW00016 - Neu, PI/PO fuer ZTVB0072
      *
      *   Rev 1.0   09 Jul 1998 09:58:18   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0319  FUNKTION UPDATE STANDARD-OZP       *
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
      *    FÜR DIE FUNKTION DRVB0319                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBVB0072 ZPKT         *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0319.
          05 PI-DRVB0319-DATEN.
             10 PI-DRVB0319-KNR-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0319-ZPKT-ZEITPUNKT    PIC  X(026).
      *
       01 PO-DRVB0319.
          05 PO-DRVB0319-RC                   PIC  X(003).
             88 PO-DRVB0319-RC-OK                         VALUE 'OK '.
             88 PO-DRVB0319-RC-NFD                        VALUE 'NFD'.
      *
      ****** Ende Copy-Member ****************************************
