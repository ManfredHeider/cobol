      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  02.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0334.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRAN0334.CPv  $
      *
      *   Rev 2.2   May 17 2019 10:48:00   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.1   May 07 2019 11:32:02   t13ae23
      *AAN01419: Optimierung Auftrag AN205100 - capping killer
      *
      *   Rev 2.0   Oct 02 2018 11:08:24   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0334                                     *
      *    FUNKTION   : BENÖTIGT PROUKT BERECHTIGUNG                 *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    26.04.2019                                      *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01419!26.04.19!2.1 !NEU                        ! RAHNER   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0334                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0334.
          05 PI-DRAN0334-DATEN.
             10 PI-DRAN0334-PGEB-GES       PIC S9(004) COMP.
             10 PI-DRAN0334-PROD-PROD-SCHL PIC S9(004) COMP.
             10 PI-DRAN0334-ANTQ-PLAUSI    PIC S9(004) COMP.
      *
       01 PO-DRAN0334.
          05 PO-DRAN0334-RC              PIC  X(003).
             88 PO-DRAN0334-RC-OK             VALUE 'OK '.
             88 PO-DRAN0334-RC-NOK            VALUE 'NOK'.
      ******** ENDE COPY-MEMBER **************************************


