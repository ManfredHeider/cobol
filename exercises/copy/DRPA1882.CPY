      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  20.02.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1882.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1882.CPv  $
      *
      *   Rev 2.1   Apr 04 2018 15:35:54   t13ae46
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung
      *
      *   Rev 2.0   Feb 20 2018 12:45:56   t13ex44
      *Archiverstellung
      *
      *--------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1882                                     *
      *    --------------------------------------------------------  *
      *    FUNKTIONSAUFRUF <<GUELTIGER MANDANT   >>                  *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WANG                                            *
      *    DATUM:    07.03.2018                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA02579!07.03.18!2.0 ! NEU                       ! WANG     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FÜR DIE FUNKTION DRPA1882                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1882.
          05 PI-DRPA1882-DATEN.
             10 PI-DRPA1882-PGEB-GES         PIC S9(004) BINARY.
             10 PI-DRPA1882-PROD-PROD-SCHL   PIC S9(004) BINARY.
             10 PI-DRPA1882-STICHTAG         PIC  X(010).
             10 PI-DRPA1882-KNR-MANDANT      PIC S9(004) BINARY.
      *
       01 PO-DRPA1882.
          05 PO-DRPA1882-RC                  PIC  X(003).
             88 PO-DRPA1882-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1882-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1882-DATEN.
             10 PO-DRPA1882-KNR-MANDANT      PIC S9(004) BINARY.
      *

