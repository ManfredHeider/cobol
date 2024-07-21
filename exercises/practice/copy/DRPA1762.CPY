      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1762.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1762.CPv  $
      *
      *   Rev 2.1   Dec 19 2013 11:13:36   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Kollektivverträge
      *
      *   Rev 2.0   Sep 04 2013 17:00:28   t13ex72
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1762                                      *
      *    --------------------------------------------------------   *
      *    I/O BEREICH DRPA1762 KOLLEKTIV UPPA0138                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: BOETTCHER                                        *
      *    DATUM:    05.09.2013                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!05.09.13! 1.0! NEU                       ! BOETTCHER*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHAELT DIE EIN- UND AUSGABEPARAMETER        *
      *    FUER DIE FUNKTION DRPA1738                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1762.
          05 PI-DRPA1762-DATEN.
             10 PI-DRPA1762-PGEB-GES           PIC S9(004) BINARY.
             10 PI-DRPA1762-SAM-NR             PIC S9(009) BINARY.
             10 PI-DRPA1762-PROD-PROD-SCHL     PIC S9(004) BINARY.
             10 PI-DRPA1762-TARIFSCHL-GRP      PIC S9(009) BINARY.
             10 PI-DRPA1762-TARIFSCHL          PIC S9(009) BINARY.
             10 PI-DRPA1762-KNR-TARIF-KZ       PIC S9(004) BINARY.
             10 PI-DRPA1762-LAUFZEIT           PIC S9(004) BINARY.
             10 PI-DRPA1762-PMPT-FAELLIG-VON   PIC  X(010).
             10 PI-DRPA1762-URSPR-POL-DAT      PIC  X(010).
      *
       01 PO-DRPA1762.
          05 PO-DRPA1762-RC                    PIC  X(003).
             88 PO-DRPA1762-RC-OK              VALUE 'OK '.
             88 PO-DRPA1762-RC-MELDUNGSFEHLER  VALUE 'MFE'.
             88 PO-DRPA1762-RC-NOT-FOUND       VALUE 'NFD'.
      *
          05 PO-DRPA1762-DATEN.
             10 PO-DRPA1762-KUERZ-ART          PIC X(01).
                88 PO-DRPA1762-KUERZ-ART-ALT   VALUE 'A'.
                88 PO-DRPA1762-KUERZ-ART-NEU   VALUE 'N'.
                88 PO-DRPA1762-KUERZ-ART-KEINE VALUE 'K'.
             10 PO-DRPA1762-KUERZ-SATZ       PIC S9(009)V9(6) COMP-3.
      ******* ENDE COPY-MEMBER **************************************
