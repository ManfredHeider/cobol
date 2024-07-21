      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0397.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0397.cpv  $
      *
      *   Rev 2.1   Jun 10 1999 11:17:34   T13AE34
      *APA00012-SST Daisy
      *
      *   Rev 2.0   Mar 17 1999 11:00:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0397  FUNKTION INSERT STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    28.05.1999                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!19.06.98!2.1 ! NEUERSTELLUNG             ! M.HEIDER *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FUER DIE FUNKTION DRVB0397                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  INSERT  <<<       TABELLE      TBVB0072 ZPKT         *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0397.
          05 PI-DRVB0397-DATEN.
             10 PI-DRVB0397-KNR-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0397-ZPKT-ZEITPUNKT    PIC  X(026).
             10 PI-DRVB0397-KTY-STEUER-INFO   PIC S9(004) BINARY.
             10 PI-DRVB0397-ZPKT-VERARB-KZ    PIC  X(001).
             10 PI-DRVB0397-ZPKT-LAST-UPDATE  PIC  X(026).
             10 PI-DRVB0397-ZPKT-KENNUNG      PIC  X(008).
      *
       01 PO-DRVB0397.
          05 PO-DRVB0397-RC                   PIC  X(003).
             88 PO-DRVB0397-RC-OK                         VALUE 'OK '.
             88 PO-DRVB0397-RC-DUPLICATE-KEY              VALUE 'DVA'.
      *
      ****** ENDE COPY-MEMBER ****************************************
