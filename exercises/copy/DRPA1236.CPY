      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1236.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1236.cpv  $
      *
      *   Rev 2.1   May 14 1999 09:06:04   T01JM2S
      *APA00012 - NEUE FUNKTION DRPA1229
      *
      *   Rev 2.0   Mar 30 1999 08:44:10   P13SP99
      *Neuerstellung
      *
      *   Rev 2.0   Mar 05 1999 09:20:30   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1236                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<ERM. GÜLTIGE GES    >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    12.05.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!12.05.99!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1236                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1236.
          05 PI-DRPA1236-DATEN.
             10 PI-DRPA1236-PROGRAMM                PIC  X(008).
             10 PI-DRPA1236-UFKT                    PIC S9(004) BINARY.
      *
       01 PO-DRPA1236.
          05 PO-DRPA1236-RC                         PIC  X(003).
             88 PO-DRPA1236-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1236-RC-EOF                        VALUE 'EOF'.
             88 PO-DRPA1236-RC-LEER                       VALUE 'LER'.
          05 PO-DRPA1236-DATEN.
             10 PO-DRPA1236-PGEB-GES                PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************
