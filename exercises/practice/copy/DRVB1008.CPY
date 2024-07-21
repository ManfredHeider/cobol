      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.02.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1008.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1008.CPv  $
      *
      *   Rev 2.1   Apr 05 2011 09:00:02   t13ex43
      *AVB01132: VB56 - Einrichten einer betreuten Direktion
      *
      *   Rev 2.0   Feb 15 2011 16:10:22   T13EX43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1008                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0023                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    15.02.2011                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB01123!15.02.11!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1008 - ERMITTELN AKTUELLER EDD-SATZ FÜR VBNR  *
      *                                                               *
      *****************************************************************

       01 PI-DRVB1008.
          05 PI-DRVB1008-DATEN.
             10 PI-DRVB1008-ED-ST-NR           PIC S9(09)       BINARY.
             10 PI-DRVB1008-EDD-BEZUGS-NR      PIC S9(09)       BINARY.

       01 PO-DRVB1008.
         03 PO-DRVB1008-RC                     PIC  X(03).
             88 PO-DRVB1008-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1008-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1008-DATEN.
      **** ACHTUNG: IN TBVB0023 STEHT BVH-UNTST-NR AN LETZTER STELLE
          05 PO-DRVB1008-NI-VIVBB023.
             10 NI-DRVB1008-ED-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1008-EDD-BEZUGS-NR      PIC S9(04)       BINARY.
             10 NI-DRVB1008-EDD-DAT-VON        PIC S9(04)       BINARY.
             10 NI-DRVB1008-EDD-LAST-UPDATE    PIC S9(04)       BINARY.
             10 NI-DRVB1008-EDD-DAT-BIS        PIC S9(04)       BINARY.
             10 NI-DRVB1008-BVH-UNTST-NR       PIC S9(04)       BINARY.
             10 NI-DRVB1008-KTY-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1008-KNR-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1008-KTY-EDD-URSACHE    PIC S9(04)       BINARY.
             10 NI-DRVB1008-KNR-EDD-URSACHE    PIC S9(04)       BINARY.
          05 PO-DRVB1008-DCL-VIVBB023.
             10 PO-DRVB1008-ED-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1008-EDD-BEZUGS-NR      PIC S9(09)       BINARY.
             10 PO-DRVB1008-EDD-DAT-VON        PIC  X(26).
             10 PO-DRVB1008-EDD-LAST-UPDATE    PIC  X(26).
             10 PO-DRVB1008-EDD-DAT-BIS        PIC  X(10).
             10 PO-DRVB1008-BVH-UNTST-NR       PIC S9(09)       BINARY.
             10 PO-DRVB1008-KTY-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1008-KNR-BEZUGS-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1008-KTY-EDD-URSACHE    PIC S9(04)       BINARY.
             10 PO-DRVB1008-KNR-EDD-URSACHE    PIC S9(04)       BINARY.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1008   --------------------------*
      *---------------------------------------------------------------*
