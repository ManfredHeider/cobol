      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  25.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1240.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1240.CPv  $
      *
      *   Rev 2.1   Jan 14 2020 13:08:48   u130213
      *AVB01580: Einführung neues ZUSTRUK-Verfahren:
      *UPRO3 - Aktio
      *nen bei neuem DIRW
      *
      *   Rev 2.0   Sep 25 2019 09:23:50   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1240                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0198                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HITZEMANN                                        *
      *    DATUM:    25.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580|25.09.19|2.0 | NEU                       | HITZEMANN *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1240 - PRÜFUNGEN BEI NEUEN DIRW-EREIGNISSEN   *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1240.
          05 PI-DRVB1240-DATEN.
             10 PI-DRVB1240-SWBAS-UNTST-NR      PIC S9(009)      BINARY.
      *
          05 PI-DRVB1240-INFO.
             10 PI-DRVB1240-COUNT-AUFRUF        PIC S9(007) COMP-3.
             10 PI-DRVB1240-LAUFDATUM           PIC  X(010).
             10 PI-DRVB1240-DAT-KORR-BIS        PIC  X(010).
             10 PI-DRVB1240-INSRT-SCHALTER      PIC  X(001) VALUE SPACE.
                88 PI-DRVB1240-INSERT-YES                   VALUE 'J'.
                88 PI-DRVB1240-INSERT-NO                    VALUE 'N'.
             10 PI-DRVB1240-DISPL-SCHALTER      PIC  X(001) VALUE SPACE.
                88 PI-DRVB1240-DISPLAY-YES                  VALUE 'J'.
                88 PI-DRVB1240-DISPLAY-CSR                  VALUE 'J',
                                                                  'C'.
                88 PI-DRVB1240-DISPLAY-SWB                  VALUE 'J',
                                                                  'S'.
                88 PI-DRVB1240-DISPLAY-NO                   VALUE 'N'.
      *
       01 PO-DRVB1240.
          05 PO-DRVB1240-RC                     PIC  X(001).
             88 PO-DRVB1240-RC-OK-INAKTIVIERT        VALUE 'X'.
             88 PO-DRVB1240-RC-OK-KEINE-AKTION       VALUE 'Y'.
             88 PO-DRVB1240-RC-FEHLER                VALUE 'Z'.
      *
          05 PO-DRVB1240-DATEN.
             10 PO-DRVB1240-ANZ-FETCH           PIC S9(009) COMP-3.
             10 PO-DRVB1240-ANZ-INAKT-UFR-SW    PIC S9(009) COMP-3.
             10 PO-DRVB1240-ANZ-INAKT-GES-SW    PIC S9(009) COMP-3.
             10 PO-DRVB1240-ANZ-INAKT-UFR-DW    PIC S9(009) COMP-3.
             10 PO-DRVB1240-ANZ-INAKT-GES-DW    PIC S9(009) COMP-3.
      *
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1240   --------------------------*
      *---------------------------------------------------------------*
