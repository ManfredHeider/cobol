      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1427.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRPA1427.CPv  $
      *
      *   Rev 2.3   Jun 10 2008 10:31:40   t13ex21
      *APA00712: FVD Stufe 1: Tabellenerweiterungen
      *
      *   Rev 2.2   19 Dec 2000 15:00:14   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.1   02 Nov 2000 13:33:30   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.0   12 Jul 2000 12:32:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1427 FUNKTION DIT-PM FORMAL PRUEFEN      *
      *                 UND NORMIEREN                                *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    25.10.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00112!25.10.00!2.1 ! NEU                       ! KLEEMEIER*
      * APA00712!08.04.08!2.2 ! PI-ERWEITERUNG UM MANDANT ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1427                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1427.
             10 PI-DRPA1427-PZVP-VERSION    PIC S9(004) BINARY.
             10 PI-DRPA1427-PEG-DATEN-DIT.
                COPY DRPA1420.
080408       10 PI-DRPA1427-KNR-MANDANT     PIC S9(004) BINARY.

       01 PO-DRPA1427.
          05 PO-DRPA1427-RC                 PIC  X(003).
             88 PO-DRPA1427-RC-OK                VALUE 'OK '.
             88 PO-DRPA1427-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
