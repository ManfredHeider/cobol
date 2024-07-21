      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  30.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1227.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1227.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 11:57:12   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Jul 30 2019 08:02:58   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1227                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS LESE-OZP ZWVB0235                   *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION UEBERGEBEN WERDEN   *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.08.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * VBL-819 !02.08.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHAELT DIE INFORMATIONEN, DIE EINMAL         *
      *    PRO FUNKTION UEBERGEBEN WERDEN --> PI/PO-BEREICH           *
      *                                                               *
      *    FUNKTION DRVB1227 - ERMITTELN SATZ AUS TABELLE FAPE        *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1227.
          05 PI-DRVB1227-DATEN.
             10 PI-DRVB1227-FAPE-LFDNR         PIC S9(04)       BINARY.
             10 PI-DRVB1227-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1227-MA-ST-NR           PIC S9(09)       BINARY.
 
       01 PO-DRVB1227.
         03 PO-DRVB1227-RC                     PIC  X(03).
             88 PO-DRVB1227-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1227-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1227-DATEN.
          05 PO-DRVB1227-NI-VIVB0235.
             10 NI-DRVB1227-FAPE-LFDNR         PIC S9(04)       BINARY.
             10 NI-DRVB1227-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1227-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-ART           PIC S9(04)       BINARY.
             10 NI-DRVB1227-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-ERM-KZ        PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-MS-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-VBP-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EH-EG-NEU     PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EH-EG-GES     PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EH-GES        PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EH-GES-KORR   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EH-GES-3MON   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-JEQF-EG       PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-JEQF-GES      PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-BES           PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-ALTERSGR-KZ   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-EINTR-KUEND   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-MAT-DAT-BIS   PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-ERF-KZ-EG     PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-ERF-KZ-GS     PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-STUFE         PIC S9(04)       BINARY.
             10 NI-DRVB1227-FAPE-STUFE-DVAG    PIC S9(04)       BINARY.
          05 PO-DRVB1227-DCL-VIVB0235.
             10 PO-DRVB1227-FAPE-LFDNR         PIC S9(04)       BINARY.
             10 PO-DRVB1227-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1227-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1227-FAPE-ART           PIC  X(01).
             10 PO-DRVB1227-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1227-FAPE-ERM-KZ        PIC  X(02).
             10 PO-DRVB1227-FAPE-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1227-FAPE-MS-NR         PIC S9(04)       BINARY.
             10 PO-DRVB1227-FAPE-VBP-NR        PIC S9(04)       BINARY.
             10 PO-DRVB1227-FAPE-EH-EG-NEU     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1227-FAPE-EH-EG-GES     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1227-FAPE-EH-GES        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1227-FAPE-EH-GES-KORR   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1227-FAPE-EH-GES-3MON   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1227-FAPE-JEQF-EG       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1227-FAPE-JEQF-GES      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1227-FAPE-BES           PIC S9(04)       BINARY.
             10 PO-DRVB1227-FAPE-ALTERSGR-KZ   PIC  X(01).
             10 PO-DRVB1227-FAPE-EINTR-KUEND   PIC  X(10).
             10 PO-DRVB1227-FAPE-MAT-DAT-BIS   PIC  X(10).
             10 PO-DRVB1227-FAPE-ERF-KZ-EG     PIC  X(01).
             10 PO-DRVB1227-FAPE-ERF-KZ-GS     PIC  X(01).
             10 PO-DRVB1227-FAPE-STUFE         PIC  X(01).
             10 PO-DRVB1227-FAPE-STUFE-DVAG    PIC  X(01).
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1227   --------------------------*
      *---------------------------------------------------------------*

