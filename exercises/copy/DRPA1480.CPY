      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1480.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1480.CPv  $
      *
      *   Rev 2.1   19 Mar 2001 09:16:06   T13AE23
      *AAN00292 - PEW Banken -
      *
      *   Rev 2.0   09 Jan 2001 10:22:20   p13sp16
      *Archiverstellung
      *****************************************************************
      *    COPY-MEMBER: DRPA1480                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRPA1480 IM UPPA0090        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                           *
      *    DATUM:    08.03.2001                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN00292!08.03.01!2.0 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRPA1480: MIGRATIONSDATUM-PEW ZU PGEB-GES
      *                               ERMITTLEN
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
      *
       01  PI-DRPA1480.
           05  PI-DRPA1480-DATEN.
               10 PI-DRPA1480-PGEB-GES            PIC  S9(004) COMP.
      *
       01  PO-DRPA1480.
           05  PO-DRPA1480-RC                     PIC  X(003).
               88 PO-DRPA1480-RC-OK                    VALUE 'OK '.
               88 PO-DRPA1480-RC-NOT-FOUND             VALUE 'NFD'.
           05  PO-DRPA1480-DATEN.
               10 PO-DRPA1480-MIGRAT-DAT          PIC X(010).
      *
      *---------------------------------------------------------------*
