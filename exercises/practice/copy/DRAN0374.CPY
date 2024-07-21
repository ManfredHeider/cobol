      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.11.2020
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAN0374.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAN0374.CPv  $
      *
      *   Rev 2.1   Nov 30 2020 12:15:58   t13ae34
      *AAN01492: Projekt Testoptimierung im eingereichten Geschäft 
      *2020 - AP3: Testautomatisierung
      *
      *   Rev 2.0   Nov 24 2020 13:52:40   t13ae34
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRAN0374                                     *
      *    FUNKTION   : Ermitteln Antragsnummer + PIN FÜR TEST TOSCA *
      *                                                              *
      *    -------------------------------------------------------   *
      *    ERSTELLT: M.HEIDER                                        *
      *    DATUM:    2020-11                                         *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01492!26.11.20!2.1 !NEU                        !HEIDER    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRAN0374                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRAN0374.
          05 PI-DRAN0374-DATEN.
             10 PI-DRAN0374-PGEB-GES             PIC S9(004) COMP.
      *
       01 PO-DRAN0374.
          05 PO-DRAN0374-RC                      PIC  X(003).
             88 PO-DRAN0374-RC-OK                     VALUE 'OK '.
             88 PO-DRAN0374-RC-NFD                    VALUE 'NFD'.
          05 PO-DRAN0374-DATEN.
             10 PO-DRAN0374-ANTA-NR              PIC S9(009) COMP.
             10 PO-DRAN0374-PZ                   PIC  X(002).
      ******** ENDE COPY-MEMBER **************************************


