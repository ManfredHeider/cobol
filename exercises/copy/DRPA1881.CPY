      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1881.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1881.CPv  $
      *
      *   Rev 2.1   Apr 05 2018 23:31:20   t13ex44
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSC
      *
      *   Rev 2.0   Feb 20 2018 10:46:44   t13ex44
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1881                                      *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF <<GUELTIGER MANDANT   >>                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHEIDEL                                         *
      *    DATUM:    16.02.2018                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!16.02.18!2.0 ! NEU                       ! SCHEIDEL *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FÜR DIE FUNKTION DRPA1881                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1881.
          05 PI-DRPA1881-DATEN.
             10 PI-DRPA1881-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1881-PGTA-TARIF       PIC  X(015).
             10 PI-DRPA1881-STICHTAG         PIC  X(010).
             10 PI-DRPA1881-KNR-MANDANT      PIC S9(004) BINARY.
      *
       01 PO-DRPA1881.
          05 PO-DRPA1881-RC                  PIC  X(003).
             88 PO-DRPA1881-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1881-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1881-DATEN.
             10 PO-DRPA1881-KNR-MANDANT      PIC S9(004) BINARY.
      *

