      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1229.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1229.CPv  $
      *
      *   Rev 2.1   May 14 1999 09:01:02   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1229
      *
      *   Rev 2.0   Mar 30 1999 08:44:08   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1229                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<KONTEN ABSCHL. GES  >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    10.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!10.05.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1229                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1229.
          05 PI-DRPA1229-DATEN.
             10 PI-DRPA1229-PGEB-GES                PIC S9(004) BINARY.
      *
       01 PO-DRPA1229.
          05 PO-DRPA1229-RC                         PIC  X(003).
             88 PO-DRPA1229-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1229-RC-KEINE-AENDERUNG            VALUE 'KAE'.
      *
      ******* ENDE COPY-MEMBER **************************************
