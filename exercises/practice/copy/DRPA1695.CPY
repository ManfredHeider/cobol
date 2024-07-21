      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.02.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1695.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1695.CPv  $
      *
      *   Rev 2.1   Jun 14 2010 10:02:08   t13ae23
      *APA01000: PIN-Änderungen AMV nach Aachen
      *
      *   Rev 2.0   Feb 09 2010 11:07:44   t13ae23
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
      *    FÜR DIE FUNKTION DRPA1695                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      *    >>>  UPDATE  <<<       TABELLE PPIA TBPA0136              *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1695.
          05 PI-DRPA1695-DATEN.
             10 PI-DRPA1695-LAST-UPDATE             PIC  X(026).
             10 PI-DRPA1695-PPIA-DATEN.
                15 PI-DRPA1695-PPIA-PGEB-GES        PIC S9(004) COMP.
                15 PI-DRPA1695-PPIA-VERTRAGSNUM     PIC X(015).
                15 PI-DRPA1695-PPIA-PIN             PIC S9(009) COMP.
                15 PI-DRPA1695-PPIA-MELDE-KZ        PIC X(001).
                15 PI-DRPA1695-PPIA-MELDUNG-VON     PIC X(008).
      *
       01 PO-DRPA1695.
          05 PO-DRPA1695-RC                         PIC  X(003).
             88 PO-DRPA1695-RC-OK                          VALUE 'OK '.
             88 PO-DRPA1695-RC-CHANGED                     VALUE 'CHA'.
             88 PO-DRPA1695-RC-NOT-FOUND                   VALUE 'NFD'.
      *
      ****** ENDE COPY-MEMBER ****************************************
