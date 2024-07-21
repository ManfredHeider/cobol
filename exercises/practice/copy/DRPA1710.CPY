      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1710.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1710.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:50   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Mar 29 2010 15:48:08   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1695  FUNKTION UPDATE STANDARD-OZP       *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01000!15.06.10!2.1 ! NEUERSTELLUNG             ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1710                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  MASSENUPDATE MELDEKENNZEICHEN  TBPA0136              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1710.
          05 PI-DRPA1710-DATEN.
             10 PI-DRPA1710-PPIA-DATEN.
                15 PI-DRPA1710-PPIA-PGEB-GES        PIC S9(004) COMP.
      *
       01 PO-DRPA1710.
          05 PO-DRPA1710-RC                         PIC  X(003).
             88 PO-DRPA1710-RC-OK                          VALUE 'OK '.
             88 PO-DRPA1710-RC-NOT-FOUND                   VALUE 'NFD'.

