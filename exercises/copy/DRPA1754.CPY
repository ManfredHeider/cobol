      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.08.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1754.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1754.CPv  $
      *
      *   Rev 2.1   Dec 20 2013 10:12:12   t13ae37
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): SST-Abrechnung
      *
      *   Rev 2.0   Aug 06 2013 10:28:42   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1754                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<LIST ZU PVPT (SORTIERT) >>               *
      *    --------------------------------------------------------   *
      *    ERSTELLT: STORCH                                           *
      *    DATUM:    30.06.1998                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                ! NAME     *
      * -------------------------------------------------------------*
      * APA00012!30.06.98!01.01!NEU                       ! STORCH   *
      *         !        !     !                          !          *
      *         !        !     !                          !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1754                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1754.
          05 PI-DRPA1754-PROGRAMM            PIC  X(008).
          05 PI-DRPA1754-UFKT-CODE           PIC  9(002).
          05 PI-DRPA1754-DATEN.
             10 PI-DRPA1754-PRINTE           PIC S9(015) COMP-3.
             10 PI-DRPA1754-PGEB-GES         PIC S9(004) COMP.
      *
       01 PO-DRPA1754.
          05 PO-DRPA1754-RC                  PIC  X(003).
             88 PO-DRPA1754-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1754-RC-EOF                VALUE 'EOF'.
             88 PO-DRPA1754-RC-LEER               VALUE 'LER'.
             88 PO-DRPA1754-RC-NOT-FOUND          VALUE 'NFD'.
          05 PO-DRPA1754-DATEN.
             10 PO-DRPA1754-HT-PROD-PROD-SCHL
                                             PIC S9(04) COMP.

      *---------------------------------------------------------------*
