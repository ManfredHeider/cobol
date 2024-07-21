      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1225.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1225.CPv  $
      *
      *   Rev 2.1   May 06 1999 08:46:22   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1225
      *
      *   Rev 2.0   Mar 30 1999 08:44:06   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:26   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1225                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. PVW/DIALOG 2   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    05.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!05.05.99! 01 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1225                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1225.
          05 PI-DRPA1225-DATEN.
             10 PI-DRPA1225-MIN-DATEN-KONTR.
                COPY DRPA1226.
      *
       01 PO-DRPA1225.
          05 PO-DRPA1225-RC                         PIC  X(003).
             88 PO-DRPA1225-RC-OK                         VALUE 'OK '.
          05 PO-DRPA1225-DATEN.
             10 PO-DRPA1225-MAX-USED-KOFV-TEIL      PIC S9(004) BINARY.
             10 PO-DRPA1225-KOFV-TEIL               OCCURS 2.
                COPY DRPA0847.
      *
      ******* ENDE COPY-MEMBER **************************************
