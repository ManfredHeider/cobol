      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.09.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1761.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRPA1761.CPv  $
      *
      *   Rev 2.1   Dec 19 2013 11:13:26   t13ex72
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Kollektivverträge
      *
      *   Rev 2.0   Sep 04 2013 17:00:28   t13ex72
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRPA1761                                      *
      *    --------------------------------------------------------   *
      *    I/O BEREICH DRPA1761 KOLLEKTIVVERTRÃ„GE UPPA0138            *
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
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRPA1738                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      ****************************************************************
      *
       01 PI-DRPA1761.
          05 PI-DRPA1761-DATEN.
             10 PI-DRPA1761-VERM-GES           PIC S9(004) BINARY.
             10 PI-DRPA1761-SAM-NR             PIC S9(009) BINARY.
             10 PI-DRPA1761-URSPR-POL-DAT      PIC X(010).
      *
       01 PO-DRPA1761.
          05 PO-DRPA1761-RC                    PIC  X(003).
             88 PO-DRPA1761-RC-OK              VALUE 'OK '.
             88 PO-DRPA1761-RC-MELDUNGSFEHLER  VALUE 'MFE'.
             88 PO-DRPA1761-RC-NOT-FOUND       VALUE 'NFD'.

          05 PO-DRPA1761-DATEN.
             10 PO-DRPA1761-VNR-ALT            PIC  X(015).
             10 PO-DRPA1761-PVER-ZUSTAND       PIC  9(002).
      ******* ENDE COPY-MEMBER **************************************
