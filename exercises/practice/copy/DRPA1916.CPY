      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  13.05.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1916.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1916.CPv  $
      *
      *   Rev 2.2   May 27 2019 12:24:26   t13ex44
      *APA02855: Umstellung Haustarif-Verträge AMV
      *
      *   Rev 2.1   May 17 2019 14:42:58   t13ex44
      *APA02855: Umstellung Haustarif-Verträge AMV
      *
      *   Rev 2.0   May 13 2019 11:01:58   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1916                                     *
      *    --------------------------------------------------------  *
      *    FUNKTIONSAUFRUF <<LESEN ABGRENZUNGDATUM>>                 *
      *    --------------------------------------------------------  *
      *    ERSTELLT: SCHEIDEL                                        *
      *    DATUM:    13.05.2019                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02855!13.05.19!2.0 ! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FÜR DIE FUNKTION DRPA1916                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1916.
          05 PI-DRPA1916-DATEN.
             10 PI-DRPA1916-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1916-PROD-PROD-SCHL   PIC S9(004) BINARY.
      *
       01 PO-DRPA1916.
          05 PO-DRPA1916-RC                  PIC  X(003).
             88 PO-DRPA1916-RC-OK                 VALUE 'OK '.
             88 PO-DRPA1916-RC-NOT-FOUND          VALUE 'NFD'.
          05 PO-DRPA1916-DATEN.
             10 PO-DRPA1916-PROD-DAT-ABGRENZ PIC  X(010).
      *


