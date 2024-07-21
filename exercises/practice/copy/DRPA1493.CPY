      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1493.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1493.CPv  $
      *
      *   Rev 2.1   17 May 2001 13:29:58   t13ex43
      *APA00132 : Check Batchbearbeitungsfunktion
      *
      *   Rev 2.0   10 Apr 2001 08:42:20   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1493 FUNKTION                            *
      *                          CHECK BATCHBEARBEITUNGSFUNKTION     *
      *    -------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                       *
      *    DATUM:    15.05.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00132!15.05.01!2.01! NEU                       ! KLEEMEIER*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1493                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1493.
          05 PI-DRPA1493-PGEB-GES                PIC S9(004) BINARY.

          05 PI-DRPA1493-KNR-BAT-BEARB OCCURS 20.
             10 PI-DRPA1493-KNR-BAT-BEARB-FKT    PIC S9(004) BINARY.

       01 PO-DRPA1493.
          05 PO-DRPA1493-RC                      PIC  X(003).
             88 PO-DRPA1493-RC-OK                VALUE 'OK '.

          05 PO-DRPA1493-KNR-BAT-BEARB OCCURS 20.
             10 PO-DRPA1493-KNR-BAT-BEARB-FKT    PIC S9(004) BINARY.
             10 PO-DRPA1493-KNR-BAT-FKT-ZUL      PIC S9(004) BINARY.
             10 PO-DRPA1493-KONF-Z-INFO          PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
