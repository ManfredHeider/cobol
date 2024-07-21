      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.02.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1733.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1733.CPv  $
      *
      *   Rev 2.1   Aug 21 2013 15:34:44   t13ae23
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Sonstige Anpassungen
      *
      *   Rev 2.0   Feb 06 2013 11:10:40   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1733 FUNKTION PRÜFEN PRODUKT AUF NEUE    *
      *                          PGRP_ID                             *
      *    -------------------------------------------------------   *
      *    ERSTELLT: RAHNER                                          *
      *    DATUM:    23.07.2013                                      *
      ****************************************************************
      *    VERSION   2.1                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!21.08.13! 2.1! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1733                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRPA1733.
          05 PI-DRPA1733-PZVP-VERSION            PIC S9(004) BINARY.
          05 PI-DRPA1733-DATEN.
             10 PI-DRPA1733-PGEB-GES             PIC S9(004) BINARY.
             10 PI-DRPA1733-PROD-PROD-SCHL       PIC S9(004) BINARY.
             10 PI-DRPA1733-STICHTAG             PIC  X(010).
      *
       01 PO-DRPA1733.
          05 PO-DRPA1733-RC                      PIC  X(003).
             88 PO-DRPA1733-RC-OK                     VALUE 'OK '.
             88 PO-DRPA1733-RC-NOT-FOUND              VALUE 'NFD'.
          05 PO-DRPA1733-DATEN.
             10 PO-DRPA1733-KNR-PGRP-ID          PIC S9(004) BINARY.
      *

