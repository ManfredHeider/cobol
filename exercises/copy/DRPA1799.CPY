      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.11.2014
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1799.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1799.CPv  $
      *
      *   Rev 2.1   Nov 17 2014 08:55:16   t13ae46
      *APA01895: Ges.50 Gekürzte EQF-Frist 3 Jahre
      *
      *   Rev 2.0   Nov 05 2014 15:48:08   t13ae46
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1799 FUNKTION PRÜFEN PRODUKT AUF NEUE    *
      *                          PGRP_ID                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    06.11.2014                                      *
      ****************************************************************
      *    VERSION   2.1                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01895!06.11.14! 2.1! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1799                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1799.
          05 PI-DRPA1799-DATEN.
             10 PI-DRPA1799-PGEB-GES             PIC S9(004) BINARY.
             10 PI-DRPA1799-PROD-PROD-SCHL       PIC S9(004) BINARY.
             10 PI-DRPA1799-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA1799.
          05 PO-DRPA1799-RC                      PIC  X(003).
             88 PO-DRPA1799-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1799-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRPA1799-DATEN.
             10 PO-DRPA1799-KNR-ST-NEUTRAL-36M   PIC S9(004) BINARY.
      *

