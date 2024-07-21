      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1237.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1237.CPv  $
      *
      *   Rev 2.1   May 17 1999 18:31:10   T01JM2S
      *APA00012 - NEUE FUNKTIONEN DRPA1237, DRPA1238
      *
      *   Rev 2.0   Mar 30 1999 08:44:10   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1237                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. PVW/BATCH ZUK 1>>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    14.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!14.05.99! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1237                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1237.
          05 PI-DRPA1237-DATEN.
             10 PI-DRPA1237-MIN-DATEN-KONTR.
                COPY DRPA1226.
      *
       01 PO-DRPA1237.
          05 PO-DRPA1237-RC                         PIC  X(003).
             88 PO-DRPA1237-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1237-DATEN.
             10 PO-DRPA1237-KOFV-TEIL.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
