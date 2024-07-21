      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  24.08.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1909.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1909.CPv  $
      *
      *   Rev 2.1   Sep 07 2018 12:04:10   t13ae46
      *APA02699: VM sollen bei bestimmten Produkten nicht der Provi
      *sionsstruktur berücksichtigt werden 
      *
      *   Rev 2.0   Aug 24 2018 14:46:22   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1909 FUNKTION PRÜFEN PRODUKT AUF NEUE    *
      *                          PGRP_ID                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    24.08.2018                                      *
      ****************************************************************
      *    VERSION   2.1                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02699!24.08.18! 2.1! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1909                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1909.
          05 PI-DRPA1909-DATEN.
             10 PI-DRPA1909-PGEB-GES             PIC S9(004) BINARY.
             10 PI-DRPA1909-PROD-PROD-SCHL       PIC S9(004) BINARY.
      *
       01 PO-DRPA1909.
          05 PO-DRPA1909-RC                      PIC  X(003).
             88 PO-DRPA1909-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1909-RC-NOT-FOUND              VALUE 'NFD'.
      *



