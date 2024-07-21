      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  23.10.2018
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2018 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1208.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1208.CPv  $
      *
      *   Rev 2.1   Dec 14 2018 15:36:42   t13ex18
      *ASL00602: Redesign Aufbauversorgung Zugriffsmodul für VIVBA1
      *14 (gbu)
      *
      *   Rev 2.0   Oct 23 2018 09:11:28   T13EX18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1208                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVBA114                    *
      *    MIT DEN DATEN, DIE EINMAL PRO FUNKTION ÜBERGEBEN WERDEN    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    23.10.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00602!23.10.18!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE INFORMATIONEN, DIE EINMAL          *
      *    PRO FUNKTION ÜBERGEBEN WERDEN --> PI/PO-BEREICH            *
      *                                                               *
      *    FUNKTION DRVB1208 - ERMITTELN SATZ AUS TABELLE ALIAS    *
      *                                                               *
      *****************************************************************
 
       01 PI-DRVB1208.
          05 PI-DRVB1208-DATEN.
             10 PI-DRVB1208-VSWE-LFDNR         PIC S9(04)       BINARY.
             10 PI-DRVB1208-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PI-DRVB1208-VSWE-LEISTUNG      PIC  X(01).
             10 PI-DRVB1208-MA-ST-NR           PIC S9(09)       BINARY.
 
       01 PO-DRVB1208.
         03 PO-DRVB1208-RC                     PIC  X(03).
             88 PO-DRVB1208-RC-OK                   VALUE 'OK '.
             88 PO-DRVB1208-RC-NOT-FOUND            VALUE 'NFD'.
         03 PO-DRVB1208-DATEN.
          05 PO-DRVB1208-NI-VIVBA114.
             10 NI-DRVB1208-VSWE-LFDNR         PIC S9(04)       BINARY.
             10 NI-DRVB1208-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1208-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-LEISTUNG      PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-ART           PIC S9(04)       BINARY.
             10 NI-DRVB1208-MA-ST-NR           PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-ERF-KZ        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-LAST-UPDATE   PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-MS-NR         PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VBP-NR        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-EG-NEU     PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-EG-KORR    PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-EG-GES     PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-GRP        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-GRP-KORR   PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-GES        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-EH-GES-KORR   PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-JEQF-EG       PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-JEQF-GRP      PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-JEQF-GES      PIC S9(04)       BINARY.
             10 NI-DRVB1208-MAT-DAT-BIS        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-BES           PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-NEU-KZ        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VERTR-KZ      PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-BES-ZUL-KZ    PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-ALTERSGR-KZ   PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VERS-NR-LV    PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VERSB-LV      PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VERS-NR-RV    PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-VERSB-RV      PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-ANZ-ED        PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-AV-BETRAG     PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-AV-BETR-ERM   PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-AV-BETR-BS    PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-AV-BETR-AG-GV PIC S9(04)       BINARY.
             10 NI-DRVB1208-VSWE-AV-BETR-AG-WG PIC S9(04)       BINARY.
          05 PO-DRVB1208-DCL-VIVBA114.
             10 PO-DRVB1208-VSWE-LFDNR         PIC S9(04)       BINARY.
             10 PO-DRVB1208-KTY-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1208-KNR-GESELL-TYP     PIC S9(04)       BINARY.
             10 PO-DRVB1208-VSWE-LEISTUNG      PIC  X(01).
             10 PO-DRVB1208-VSWE-ART           PIC  X(01).
             10 PO-DRVB1208-MA-ST-NR           PIC S9(09)       BINARY.
             10 PO-DRVB1208-VSWE-ERF-KZ        PIC  X(02).
             10 PO-DRVB1208-VSWE-LAST-UPDATE   PIC  X(26).
             10 PO-DRVB1208-VSWE-MS-NR         PIC S9(04)       BINARY.
             10 PO-DRVB1208-VSWE-VBP-NR        PIC S9(04)       BINARY.
             10 PO-DRVB1208-VSWE-EH-EG-NEU     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-EG-KORR    PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-EG-GES     PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-GRP        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-GRP-KORR   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-GES        PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-EH-GES-KORR   PIC S9(09)V9(01) COMP-3.
             10 PO-DRVB1208-VSWE-JEQF-EG       PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-JEQF-GRP      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-JEQF-GES      PIC S9(03)V9(02) COMP-3.
             10 PO-DRVB1208-MAT-DAT-BIS        PIC  X(10).
             10 PO-DRVB1208-VSWE-BES           PIC S9(04)       BINARY.
             10 PO-DRVB1208-VSWE-NEU-KZ        PIC  X(03).
             10 PO-DRVB1208-VSWE-VERTR-KZ      PIC  X(01).
             10 PO-DRVB1208-VSWE-BES-ZUL-KZ    PIC  X(01).
             10 PO-DRVB1208-VSWE-ALTERSGR-KZ   PIC  X(01).
             10 PO-DRVB1208-VSWE-VERS-NR-LV    PIC S9(15)       COMP-3.
             10 PO-DRVB1208-VSWE-VERSB-LV      PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-VERS-NR-RV    PIC S9(15)       COMP-3.
             10 PO-DRVB1208-VSWE-VERSB-RV      PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-ANZ-ED        PIC S9(04)       BINARY.
             10 PO-DRVB1208-VSWE-AV-BETRAG     PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-AV-BETR-ERM   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-AV-BETR-BS    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-AV-BETR-AG-GV PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB1208-VSWE-AV-BETR-AG-WG PIC S9(09)V9(02) COMP-3.
      *---------------------------------------------------------------*
      *----   ENDE  COPY-MEMBER  DRVB1208   --------------------------*
      *---------------------------------------------------------------*

