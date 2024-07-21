      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1006.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1006.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 08:59:52   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.0   Feb 15 2011 16:10:10   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1006                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0018                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.02.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01132!15.02.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1006 - ERMITTELN AKTUELLER DZ-SATZ FÜR VBNR   *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1006.
          05 PI-DRVB1006-DATEN.
             10 PI-DRVB1006-ST-NR              PIC S9(09)       BINARY.

       01 PO-DRVB1006.
         03 PO-DRVB1006-RC                     PIC  X(03).
             88 PO-DRVB1006-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1006-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1006-DATEN.
          05 PO-DRVB1006-NI-VIVB0018.
             10 NI-DRVB1006-ST-NR              PIC S9(04)       BINARY.
             10 NI-DRVB1006-DIR-NR             PIC S9(04)       BINARY.
             10 NI-DRVB1006-DZ-DAT-VON         PIC S9(04)       BINARY.
             10 NI-DRVB1006-DZ-LAST-UPDATE     PIC S9(04)       BINARY.
             10 NI-DRVB1006-DZ-DAT-BIS         PIC S9(04)       BINARY.
          05 PO-DRVB1006-DCL-VIVB0018.
             10 PO-DRVB1006-ST-NR              PIC S9(09)       BINARY.
             10 PO-DRVB1006-DIR-NR             PIC S9(04)       BINARY.
             10 PO-DRVB1006-DZ-DAT-VON         PIC  X(26).
             10 PO-DRVB1006-DZ-LAST-UPDATE     PIC  X(26).
             10 PO-DRVB1006-DZ-DAT-BIS         PIC  X(10).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1006   --------------------------*
      *---------------------------------------------------------------*
