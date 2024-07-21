      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1428.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRPA1428.CPv  $
      *
      *   Rev 2.3   29 Jan 2001 08:46:46   T13EX43
      *FPA00230 : dam-PIN mit 9xxxxxx akzeptieren.
      *
      *   Rev 2.2   19 Dec 2000 15:00:34   T13EX43
      *APA00111 : Wiederholung NTPUT (identisch mit Vorversion)
      *
      *   Rev 2.1   02 Nov 2000 13:33:40   T13EX43
      *APA00112 : DIT-PEG umsetzen und einführen
      *
      *   Rev 2.0   12 Jul 2000 12:32:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1428 FUNKTION PIN FÜR DIT PRÜFEN         *
      *                                                              *
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
      * FPA00230!26.01.01!2.3 ! VNR HINZU                 ! KLEEMEIER*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1428                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1428.
             10 PI-DRPA1428-ZVP-VERSION     PIC S9(004) BINARY.
             10 PI-DRPA1428-PGEB-GES        PIC S9(004) BINARY.
             10 PI-DRPA1428-PIN             PIC S9(009) BINARY.
260101       10 PI-DRPA1428-VNR             PIC  9(015).
             10 PI-DRPA1428-VERTR-BEGINN    PIC  X(010).

       01 PO-DRPA1428.
          05 PO-DRPA1428-RC                 PIC  X(003).
             88 PO-DRPA1428-RC-OK                VALUE 'OK '.
             88 PO-DRPA1428-RC-VSTAT-GEFUELLT    VALUE 'VSG'.
      ******** ENDE COPY-MEMBER **************************************
