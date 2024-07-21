      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1007.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1007.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 08:59:56   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.0   Feb 15 2011 16:10:18   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1007                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0019                    *
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
      *    FUNKTION DRVB1007 - ERMITTELN AKTUELLER DL-SATZ FÜR VBNR   *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1007.
          05 PI-DRVB1007-DATEN.
             10 PI-DRVB1007-DL-BEZUGS-NR       PIC S9(09)       BINARY.

       01 PO-DRVB1007.
         03 PO-DRVB1007-RC                     PIC  X(03).
             88 PO-DRVB1007-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1007-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1007-DATEN.
          05 PO-DRVB1007-NI-VIVB0019.
             10 NI-DRVB1007-DL-BEZUGS-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1007-DIR-NR             PIC S9(04)       BINARY.
             10 NI-DRVB1007-DL-DAT-VON         PIC S9(04)       BINARY.
             10 NI-DRVB1007-DL-LAST-UPDATE     PIC S9(04)       BINARY.
             10 NI-DRVB1007-DL-DAT-BIS         PIC S9(04)       BINARY.
             10 NI-DRVB1007-KTY-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1007-KNR-BEZUGS-TYP     PIC S9(04)       BINARY.
          05 PO-DRVB1007-DCL-VIVB0019.
             10 PO-DRVB1007-DL-BEZUGS-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1007-DIR-NR             PIC S9(04)       BINARY.
             10 PO-DRVB1007-DL-DAT-VON         PIC  X(26).
             10 PO-DRVB1007-DL-LAST-UPDATE     PIC  X(26).
             10 PO-DRVB1007-DL-DAT-BIS         PIC  X(10).
             10 PO-DRVB1007-KTY-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1007-KNR-BEZUGS-TYP     PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1007   --------------------------*
      *---------------------------------------------------------------*
