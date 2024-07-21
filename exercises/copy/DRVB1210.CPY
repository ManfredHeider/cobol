      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  08.01.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1210.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1210.CPv  $
      *
      *   Rev 2.1   Jan 08 2019 16:15:28   t13ae56
      *ACV00185: PGM - Vertragsrückläufer
      *
      *   Rev 2.0   Jan 08 2019 11:20:24   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1210                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0014                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    08.01.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ACV00185!08.01.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1210 - ERMITTELN MAN IN GROSS-/KLEINSCHREIBG. *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1210.
          05 PI-DRVB1210-DATEN.
             10 PI-DRVB1210-MAN-BEZUGS-NR      PIC S9(09)       COMP.

       01 PO-DRVB1210.
          05 PO-DRVB1210-RC                    PIC  X(03).
                88 PO-DRVB1210-RC-OK                VALUE 'OK '.
                88 PO-DRVB1210-RC-NOT-FOUND         VALUE 'NFD'.
          05 PO-DRVB1210-DATEN.
             10 PO-DRVB1210-MAN-ANREDE         PIC  9(04).
             10 PO-DRVB1210-MAN-NAME-GK        PIC  X(40).
             10 PO-DRVB1210-MAN-VORNAME-GK     PIC  X(20).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1210   --------------------------*
      *---------------------------------------------------------------*

