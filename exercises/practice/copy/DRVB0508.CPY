      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0508.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0508.CPv  $
      *
      *   Rev 2.1   06 Oct 2000 10:40:42   T13AE30
      * 
      *
      *   Rev 2.0   07 Sep 2000 11:32:10   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0508  FUNKTION UPDATE STANDARD-OZP       *
      *    -------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                       *
      *    DATUM:    20.09.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00067!20.09.00! 01 ! NEUERSTELLUNG             ! TH.WILLIG*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0508                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE      TBVB0072 ZPKT         *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0508.
          05 PI-DRVB0508-DATEN.
             10 PI-DRVB0508-KNR-STEUER-INFO       PIC S9(004) BINARY.
             10 PI-DRVB0508-ZPKT-ZEITPUNKT        PIC  X(026).
             10 PI-DRVB0508-KTY-STEUER-INFO       PIC S9(004) BINARY.
             10 PI-DRVB0508-ZPKT-VERARB-KZ        PIC  X(001).
             10 PI-DRVB0508-ZPKT-LAST-UPDATE      PIC  X(026).
             10 PI-DRVB0508-ZPKT-KENNUNG          PIC  X(008).
             10 PI-DRVB0508-ZPKT-ZEITPUNKT-ALT    PIC  X(026).
      *
       01 PO-DRVB0508.
          05 PO-DRVB0508-RC                       PIC  X(003).
             88 PO-DRVB0508-RC-OK                         VALUE 'OK '.
             88 PO-DRVB0508-RC-CHANGED                    VALUE 'CHA'.
             88 PO-DRVB0508-RC-DUPLICATE-VALUE            VALUE 'DVA'.
      *
      *************** Ende Copy-Member DRVB0508 ************************
