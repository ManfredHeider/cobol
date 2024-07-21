      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1392.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drpa1392.cpv  $
      *
      *   Rev 2.1   03 Aug 2000 11:30:22   T01JM2S
      *APA00083 - EINFÜHRUNG REOB
      *
      *   Rev 2.0   Apr 11 2000 16:58:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1392                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<PVW GEVO EVS REOB   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WOLFF                                            *
      *    DATUM:    13.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00083!13.06.00!2.1 ! NEU                       ! WOLFF    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1392                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1392.
          05 PI-DRPA1392-DATEN.
             10 PI-DRPA1392-PIN-VNR-AKZ             PIC S9(004) BINARY.
      *
       01 PO-DRPA1392.
          05 PO-DRPA1392-RC                         PIC  X(003).
             88 PO-DRPA1392-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1392-RC-MELDUNGSFEHLER             VALUE 'MFE'.
          05 PO-DRPA1392-DATEN.
             10 PO-DRPA1392-PVER.
                COPY DRPA0653.
             10 PO-DRPA1392-PVOR.
                COPY DRPA0614.
      *
      ******* ENDE COPY-MEMBER **************************************
