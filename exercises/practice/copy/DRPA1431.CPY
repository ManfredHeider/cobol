      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1431.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1431.CPv  $
      *
      *   Rev 2.2   Jun 10 2008 10:31:50   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.1   15 Nov 2000 16:09:10   T13EX43
      *APA00116 : Hypo-PEG umsetzen und einführen
      *
      *   Rev 2.0   12 Jul 2000 12:33:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1431 FUNKTION UWPA0047                   *
      *                 HYPO-PM FORMAL PRUEFEN UND NORMIEREN         *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    08.11.2000                                      *
      ****************************************************************
      *    VERSION   02                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00116!08.11.00!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1431                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1431.
             10 PI-DRPA1431-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1431-PEG-DATEN-HYPO.
                COPY DRPA1430.
080408       10 PI-DRPA1431-KNR-MANDANT     PIC S9(004) BINARY.
      *
       01 PO-DRPA1431.
          05 PO-DRPA1431-RC                 PIC  X(003).
             88 PO-DRPA1431-RC-OK                VALUE 'OK '.
             88 PO-DRPA1431-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      *
      ******** ENDE COPY-MEMBER **************************************
