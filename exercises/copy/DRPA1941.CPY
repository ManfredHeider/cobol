      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  18.11.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1941.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1941.CPv  $
      *
      *   Rev 2.1   Mar 17 2020 08:35:12   T13ae37
      *APA02884: Einführung "Bezugsjahr" bei allen USt.pfl. Produkt
      *en in DAISY - PVW
      *
      *   Rev 2.0   Nov 18 2019 11:39:54   t13ae37
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1941 FUNKTION UMSATZSTEUER BERECHNEN     *
      *                                    UND VERTEILEN             *
      *    --------------------------------------------------------- *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02884!19.11.19!2.1 ! NEUERSTELLUNG             ! BUCH     *
      * DSY04717!23.05.22!    ! TAGESDATUM STATT FÄLLIGK. ! BUCH     *
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
       01 PI-DRPA1941.
          05 PI-DRPA1941-DATEN.
             10 PI-DRPA1941-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1941-PMBE-PG-RECH-NR    PIC  X(030).
             10 PI-DRPA1941-PRINTE             PIC S9(015)
                                                   PACKED-DECIMAL.
      *      10 PI-DRPA1941-FAELLIG-VON        PIC  X(010).
             10 PI-DRPA1941-STICHTAG           PIC  X(010).
             10 PI-DRPA1941-PRODART            PIC S9(004) BINARY.

      *
       01 PO-DRPA1941.
          05 PO-DRPA1941-RC                    PIC  X(003).
             88 PO-DRPA1941-RC-OK                  VALUE 'OK '.
             88 PO-DRPA1941-RC-NOT-FOUND           VALUE 'NFD'.
             88 PO-DRPA1941-RC-MELDUNGSFEHLER      VALUE 'MFE'.
          05 PO-DRPA1941-DATEN.
             10 PO-DRPA1941-PMBE-BEZUGSJAHR    PIC S9(004) BINARY.
      *
      ******** ENDE COPY-MEMBER **************************************
 
 
