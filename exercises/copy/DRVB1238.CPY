      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1238.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1238.CPv  $
      *
      *   Rev 2.1   Jan 14 2020 13:11:48   t13ae53
      *AVB01580: Einführung neues ZUSTRUK-Verfahren:  UPRO2 - Aktio
      *nen bei neuem STRUKW
      *
      *   Rev 2.0   Sep 25 2019 08:54:22   t13ae53
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1238                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0197                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: THURMANN                                         *
      *    DATUM:    25.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580|25.09.19|2.0 | NEU                       | THURMANN  *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1238 - PRÜFUNGEN BEI NEUEN DIRW-EREIGNISSEN   *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1238.
          05 PI-DRVB1238-DATEN.
             10 PI-DRVB1238-SWBAS-BETR-NR      PIC S9(009)      BINARY.
             10 PI-DRVB1238-SWBAS-UNTST-NR     PIC S9(009)      BINARY.
             10 PI-DRVB1238-INSRT-SCHALTER     PIC  X(001)  VALUE SPACE.
                88 PI-DRVB1238-INSERT-YES                     VALUE 'J'.
                88 PI-DRVB1238-INSERT-NO                      VALUE 'N'.
             10 PI-DRVB1238-DISPL-SCHALTER      PIC  X(001) VALUE SPACE.
                88 PI-DRVB1238-DISPLAY-YES                  VALUE 'J'.
                88 PI-DRVB1238-DISPLAY-CSR                  VALUE 'J',
                                                                  'C'.
                88 PI-DRVB1238-DISPLAY-SWB                  VALUE 'J',
                                                                  'S'.
                88 PI-DRVB1238-DISPLAY-NO                   VALUE 'N'.
      *
          05 PI-DRVB1238-INFO.
             10 PI-DRVB1238-LAUFDATUM          PIC  X(010).
             10 PI-DRVB1238-DAT-KORR-BIS       PIC  X(010).
             10 PI-DRVB1238-COUNT-AUFRUF       PIC S9(007) COMP-3.
      *
       01 PO-DRVB1238.
          05 PO-DRVB1238-RC                    PIC  X(001).
             88 PO-DRVB1238-RC-OK                   VALUE 'Y'.
             88 PO-DRVB1238-RC-NOK                  VALUE 'N'.
          05 PO-DRVB1238-RC                    PIC  X(001).
             88 PO-DRVB1238-RC-BUILD-SWBAS          VALUE 'Y'.
             88 PO-DRVB1238-RC-NO-ACTION            VALUE 'N'.
             88 PO-DRVB1238-RC-FEHLER               VALUE 'Z'.
          05 PO-DRVB1238-DATEN.
             10 PO-DRVB1238-ANZ-FETCH          PIC S9(009) COMP-3.
             10 PO-DRVB1238-ANZ-INAKT-UFR-SW    PIC S9(009) COMP-3.
             10 PO-DRVB1238-ANZ-INAKT-GES-SW    PIC S9(009) COMP-3.
             10 PO-DRVB1238-ANZ-INAKT-UFR-DW    PIC S9(009) COMP-3.
             10 PO-DRVB1238-ANZ-INAKT-GES-DW    PIC S9(009) COMP-3.
             10 PO-DRVB1238-ANZ-DOPPELTE-SW     PIC S9(009) COMP-3.
      *
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1238   --------------------------*
      *---------------------------------------------------------------*
