      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1647.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1647.CPv  $
      *
      *   Rev 2.1   Jun 10 2005 09:49:22   t13ex44
      *APA00353: Sortierung der Datensätze im Provisionseingang
      *
      *   Rev 2.0   Jun 07 2005 14:52:40   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1647                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LESEN ZU PGV SORTIERT NACH FAELLIGKEIT   *
      *    UND VERTRAGSNUMMER>>                                       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: M.SCHEIDEL                                       *
      *    DATUM:    02.06.2005                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00353!02.06.05! 2.0! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1647                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1647.
          05 PI-DRPA1647-PROGRAMM      PIC  X(008).
          05 PI-DRPA1647-UFKT          PIC  9(002).
          05 PI-DRPA1647-DATEN.
             10 PI-DRPA1647-PDTA-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PI-DRPA1647-PGEB-GES   PIC S9(004)       BINARY.
             10 PI-DRPA1647-PMPP-PGV   PIC S9(004)       BINARY.
      *
       01 PO-DRPA1647.
          05 PO-DRPA1647-RC            PIC  X(003).
             88 PO-DRPA1647-RC-OK           VALUE 'OK '.
             88 PO-DRPA1647-RC-EOF          VALUE 'EOF'.
             88 PO-DRPA1647-RC-LEER         VALUE 'LER'.
          05 PO-DRPA1647-DATEN.
             10 PO-DRPA1647-PMPG-ID    PIC S9(015)       PACKED-DECIMAL.
             10 PO-DRPA1647-PMPG-VERSION
                                       PIC S9(004)       BINARY.
