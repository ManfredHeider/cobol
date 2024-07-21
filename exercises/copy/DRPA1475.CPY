      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1475.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1475.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:32:24   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   13 Mar 2001 16:42:16   T13EX25
      *APA00131; PEG Central
      *
      *   Rev 2.0   09 Jan 2001 10:22:12   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1475 FUNKTION CENTRAL-PM FORMAL PRUEFEN  *
      *                 UND NORMIEREN                                *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00131!27.02.01!2.1 ! NEU                       ! JÄGER    *
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1475                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1475.
             10 PI-DRPA1475-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1475-PEG-DATEN-CENTRAL.
                COPY DRPA1476.
080408       10 PI-DRPA1475-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1475.
          05 PO-DRPA1475-RC                 PIC  X(003).
             88 PO-DRPA1475-RC-OK                VALUE 'OK '.
             88 PO-DRPA1475-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
