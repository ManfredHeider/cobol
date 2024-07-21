      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  09.09.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1232.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1232.CPv  $
      *
      *   Rev 2.1   Jan 14 2020 12:57:54   u130213
      *AVB01580: Einführung neues ZUSTRUK-Verfahren: 
      * Upro1 - Rea
      *ktivierung
      *
      *   Rev 2.0   Sep 09 2019 10:47:38   u130213
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1232                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO UPVB0196                       *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: HITZEMANN                                        *
      *    DATUM:    09.09.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG | DATUM  |VER | AENDERUNG                 | NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01580|09.09.19|2.0 | NEU                       | HITZEMANN *
      *         |        |    |                           |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1232 - PRÜFEN REAKTIVIERUNG SWBAS             *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB1232.
          05 PI-DRVB1232-DATEN.
             10 PI-DRVB1232-SWBAS-BETR-NR      PIC S9(009)      BINARY.
             10 PI-DRVB1232-SWBAS-UNTST-NR     PIC S9(009)      BINARY.
             10 PI-DRVB1232-KTY-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PI-DRVB1232-KNR-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PI-DRVB1232-SWBAS-DIR-NEU      PIC S9(004)      BINARY.
      *
          05 PI-DRVB1232-INFO.
             10 PI-DRVB1232-LAUFDATUM          PIC  X(010).
             10 PI-DRVB1232-KORR-AB            PIC  X(010).
      *
       01 PO-DRVB1232.
          05 PO-DRVB1232-RC                    PIC  X(001).
             88 PO-DRVB1232-RC-REAKTIV-YES          VALUE 'Y'.
             88 PO-DRVB1232-RC-REAKTIV-NO           VALUE 'N'.
             88 PO-DRVB1232-RC-FEHLER               VALUE 'Z'.
      *
          05 PO-DRVB1232-DATEN-UFR.
             10 PO-DRVB1232-SWBAS-BETR-NR      PIC S9(009)      BINARY.
             10 PO-DRVB1232-SWBAS-UNTST-NR     PIC S9(009)      BINARY.
             10 PO-DRVB1232-SWBAS-DAT-WECHSEL  PIC  X(010).
             10 PO-DRVB1232-KTY-UST-TYP        PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-UST-TYP        PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-KORR-AB      PIC  X(010).
             10 PO-DRVB1232-SWBAS-KORR-BIS     PIC  X(010).
             10 PO-DRVB1232-SWBAS-KORR-ENDE    PIC  X(010).
             10 PO-DRVB1232-SWBAS-BASIS-AB     PIC  X(010).
             10 PO-DRVB1232-SWBAS-BASIS-BIS    PIC  X(010).
             10 PO-DRVB1232-KTY-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-DIR-NEU      PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-EBENE        PIC S9(004)      BINARY.
             10 PO-DRVB1232-KTY-KORR-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-KORR-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-KORR-DAT       PIC  X(010).
             10 PO-DRVB1232-SWBAS-EH-BASIS-12  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1232-SWBAS-ABR-MONATE   PIC S9(04)       BINARY.
             10 PO-DRVB-SWBAS-EH-MONWERT       PIC S9(09)V9(01) COMP-3.
      *
          05 PO-DRVB1232-DATEN-GES.
             10 PO-DRVB1232-SWBAS-BETR-NR      PIC S9(009)      BINARY.
             10 PO-DRVB1232-SWBAS-UNTST-NR     PIC S9(009)      BINARY.
             10 PO-DRVB1232-SWBAS-DAT-WECHSEL  PIC  X(010).
             10 PO-DRVB1232-KTY-UST-TYP        PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-UST-TYP        PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-KORR-AB      PIC  X(010).
             10 PO-DRVB1232-SWBAS-KORR-BIS     PIC  X(010).
             10 PO-DRVB1232-SWBAS-KORR-ENDE    PIC  X(010).
             10 PO-DRVB1232-SWBAS-BASIS-AB     PIC  X(010).
             10 PO-DRVB1232-SWBAS-BASIS-BIS    PIC  X(010).
             10 PO-DRVB1232-KTY-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-ZUSTRUK-TYP    PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-DIR-NEU      PIC S9(004)      BINARY.
             10 PO-DRVB1232-SWBAS-EBENE        PIC S9(004)      BINARY.
             10 PO-DRVB1232-KTY-KORR-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-KORR-TYP       PIC S9(004)      BINARY.
             10 PO-DRVB1232-KNR-KORR-DAT       PIC  X(010).
             10 PO-DRVB1232-SWBAS-EH-BASIS-12  PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1232-SWBAS-ABR-MONATE   PIC S9(04)       BINARY.
             10 PO-DRVB-SWBAS-EH-MONWERT       PIC S9(09)V9(01) COMP-3.
      *
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1232   --------------------------*
      *---------------------------------------------------------------*
