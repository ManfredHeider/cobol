      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.10.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1152.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1152.CPv  $
      *
      *   Rev 2.1   Nov 02 2016 09:52:02   t13ex30
      *ASL00559: Betreuung Versorgungsleistungen 2016 - PMSL0068 un
      *d PMSL0109 (HWO)
      *
      *   Rev 2.0   Oct 12 2016 09:47:06   t13ex30
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1152                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0114                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: H. WÖLFLIK                                       *
      *    DATUM:    12.10.2015                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00559!12.10.16!2.1 ! NEU                       ! H.WÖLFLIK *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1152 - ERMITTELN VSWE-SATZ OHNE LFDNR         *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1152.
           05  PI-DRVB1152-DATEN.
               10  PI-DRVB1152-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PI-DRVB1152-VSWE-LEISTUNG    PIC  X(01).
               10  PI-DRVB1152-VSWE-ART         PIC  X(01).
               10  PI-DRVB1152-MA-ST-NR         PIC S9(09)       BINARY.
      *
       01  PO-DRVB1152.
         03  PO-DRVB1152-RC                     PIC  X(03).
               88  PO-DRVB1152-RC-OK                 VALUE 'OK '.
               88  PO-DRVB1152-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB1152-DATEN.
           05  PO-DRVB1152-NI-VIVB0114.
               10  NI-DRVB1152-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  NI-DRVB1152-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB1152-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-LEISTUNG    PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-ART         PIC S9(04)       BINARY.
               10  NI-DRVB1152-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-ERF-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-LAST-UPDATE PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-EG-NEU   PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-EG-KORR  PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-EG-GES   PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-GRP      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-GRP-KORR PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-GES      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-EH-GES-KORR PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-JEQF-EG     PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-JEQF-GRP    PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-JEQF-GES    PIC S9(04)       BINARY.
               10  NI-DRVB1152-MAT-DAT-BIS      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-BES         PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-NEU-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VERTR-KZ    PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-BES-ZUL-KZ  PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-ALTERSGR-KZ PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VERS-NR-LV  PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VERSB-LV    PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VERS-NR-RV  PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-VERSB-RV    PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  NI-DRVB1152-VSWE-AV-BETRAG   PIC S9(04)       BINARY.
           05  PO-DRVB1152-DCL-VIVB0114.
               10  PO-DRVB1152-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  PO-DRVB1152-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB1152-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB1152-VSWE-LEISTUNG    PIC  X(01).
               10  PO-DRVB1152-VSWE-ART         PIC  X(01).
               10  PO-DRVB1152-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB1152-VSWE-ERF-KZ      PIC  X(02).
               10  PO-DRVB1152-VSWE-LAST-UPDATE PIC  X(26).
               10  PO-DRVB1152-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  PO-DRVB1152-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  PO-DRVB1152-VSWE-EH-EG-NEU   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-EG-KORR  PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-EG-GES   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-GRP      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-GRP-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-GES      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-EH-GES-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1152-VSWE-JEQF-EG     PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1152-VSWE-JEQF-GRP    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1152-VSWE-JEQF-GES    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1152-MAT-DAT-BIS      PIC  X(10).
               10  PO-DRVB1152-VSWE-BES         PIC S9(04)       BINARY.
               10  PO-DRVB1152-VSWE-NEU-KZ      PIC  X(03).
               10  PO-DRVB1152-VSWE-VERTR-KZ    PIC  X(01).
               10  PO-DRVB1152-VSWE-BES-ZUL-KZ  PIC  X(01).
               10  PO-DRVB1152-VSWE-ALTERSGR-KZ PIC  X(01).
               10  PO-DRVB1152-VSWE-VERS-NR-LV  PIC S9(15)       COMP-3.
               10  PO-DRVB1152-VSWE-VERSB-LV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1152-VSWE-VERS-NR-RV  PIC S9(15)       COMP-3.
               10  PO-DRVB1152-VSWE-VERSB-RV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1152-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  PO-DRVB1152-VSWE-AV-BETRAG   PIC S9(09)V9(02) COMP-3.
      ****
      **** ENDE COPY DRVB1152 ****************************************
      ****

