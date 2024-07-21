      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  21.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1942.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1942.CPv  $
      *
      *   Rev 2.1   Mar 17 2020 08:35:24   T13ae37
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - PVW
      *
      *   Rev 2.0   Nov 21 2019 08:59:24   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1942 FUNKTION BEZUGSJAHR ZU RECHNUNGS-   *
      *                          NUMMER ERMITTELN                    *
      *    --------------------------------------------------------- *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02884!21.11.19!2.1 ! NEUERSTELLUNG             ! BUCH     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1941                                 *
      *                                                              *
      *    ZUR ERMITTLUNG DES BEZUGSJAHRES BEI UMSATZSTEUERPFLICHT   *
      *                                                              *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1942.
          05 PI-DRPA1942-DATEN.
             10 PI-DRPA1942-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1942-PMBE-PG-RECH-NR    PIC  X(030).
             10 PI-DRPA1942-PRINTE             PIC S9(015)
                                                   PACKED-DECIMAL.

      *
       01 PO-DRPA1942.
          05 PO-DRPA1942-RC                    PIC  X(003).
             88 PO-DRPA1942-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1942-RC-NOT-FOUND           VALUE 'NFD'.
          05 PO-DRPA1942-DATEN.
             10 PO-DRPA1942-PMBE-BEZUGSJAHR    PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
