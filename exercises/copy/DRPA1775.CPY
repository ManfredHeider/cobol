      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.11.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1775.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1775.CPv  $
      *
      *   Rev 2.1   Jan 10 2014 13:28:32   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): GeVo Leben (CBO)
      *
      *   Rev 2.0   Nov 26 2013 16:12:34   t13ex72
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1775 ERMITTELN GRÖSSTE REIHENFOLGENR. ZUR *
      *                          UNR/SCHICHTNR                        *
      *    --------------------------------------------------------   *
      *    FUNKTIONSAUFRUF DRPA1775                                   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BÖTTCHER                                         *
      *    DATUM:    26.11.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!26.11.13! 01 ! NEU                       ! BÖTTCHER *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1755                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1775.
          05 PI-DRPA1775-DATEN.
             10 PI-DRPA1775-PRINTE                  PIC S9(015)
                                                        PACKED-DECIMAL.
             10 PI-DRPA1775-UNR                     PIC S9(004) BINARY.
             10 PI-DRPA1775-SCHICHT-NR              PIC S9(004) BINARY.
      *
       01 PO-DRPA1775.
          05 PO-DRPA1775-RC                         PIC  X(003).
             88 PO-DRPA1775-RC-OK                         VALUE 'OK '.
             88 PO-DRPA1775-RC-NOT-FOUND                  VALUE 'NFD'.
          05 PO-DRPA1775-DATEN.
             10 PO-DRPA1775-PMBE-RFOLGE-NR          PIC S9(004) BINARY.
      *
      ******* ENDE COPY-MEMBER **************************************


