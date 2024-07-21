      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  01.08.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0912.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0912.CPv  $
      *
      *   Rev 2.1   Sep 10 2008 13:57:22   t13ex43
      *ASL00309: PGM - Redesign Versorgungswerk (Dialog 2)
      *
      *   Rev 2.0   Aug 01 2008 15:19:56   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0912                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0114                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.08.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!01.08.08!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0912 - ERMITTELN VSWE-SATZ                    *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0912.
           05  PI-DRVB0912-DATEN.
               10  PI-DRVB0912-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  PI-DRVB0912-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PI-DRVB0912-VSWE-LEISTUNG    PIC  X(01).
               10  PI-DRVB0912-MA-ST-NR         PIC S9(09)       BINARY.
      *
       01  PO-DRVB0912.
         03  PO-DRVB0912-RC                     PIC  X(03).
               88  PO-DRVB0912-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0912-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB0912-DATEN.
           05  PO-DRVB0912-NI-VIVB0114.
               10  NI-DRVB0912-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  NI-DRVB0912-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB0912-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-LEISTUNG    PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-ART         PIC S9(04)       BINARY.
               10  NI-DRVB0912-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-ERF-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-LAST-UPDATE PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-EG-NEU   PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-EG-KORR  PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-EG-GES   PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-GRP      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-GRP-KORR PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-GES      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-EH-GES-KORR PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-JEQF-EG     PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-JEQF-GRP    PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-JEQF-GES    PIC S9(04)       BINARY.
               10  NI-DRVB0912-MAT-DAT-BIS      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-BES         PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-NEU-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VERTR-KZ    PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-BES-ZUL-KZ  PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-ALTERSGR-KZ PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VERS-NR-LV  PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VERSB-LV    PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VERS-NR-RV  PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-VERSB-RV    PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  NI-DRVB0912-VSWE-AV-BETRAG   PIC S9(04)       BINARY.
           05  PO-DRVB0912-DCL-VIVB0114.
               10  PO-DRVB0912-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  PO-DRVB0912-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB0912-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB0912-VSWE-LEISTUNG    PIC  X(01).
               10  PO-DRVB0912-VSWE-ART         PIC  X(01).
               10  PO-DRVB0912-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB0912-VSWE-ERF-KZ      PIC  X(02).
               10  PO-DRVB0912-VSWE-LAST-UPDATE PIC  X(26).
               10  PO-DRVB0912-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  PO-DRVB0912-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  PO-DRVB0912-VSWE-EH-EG-NEU   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-EG-KORR  PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-EG-GES   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-GRP      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-GRP-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-GES      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-EH-GES-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB0912-VSWE-JEQF-EG     PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0912-VSWE-JEQF-GRP    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0912-VSWE-JEQF-GES    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0912-MAT-DAT-BIS      PIC  X(10).
               10  PO-DRVB0912-VSWE-BES         PIC S9(04)       BINARY.
               10  PO-DRVB0912-VSWE-NEU-KZ      PIC  X(03).
               10  PO-DRVB0912-VSWE-VERTR-KZ    PIC  X(01).
               10  PO-DRVB0912-VSWE-BES-ZUL-KZ  PIC  X(01).
               10  PO-DRVB0912-VSWE-ALTERSGR-KZ PIC  X(01).
               10  PO-DRVB0912-VSWE-VERS-NR-LV  PIC S9(15)       COMP-3.
               10  PO-DRVB0912-VSWE-VERSB-LV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB0912-VSWE-VERS-NR-RV  PIC S9(15)       COMP-3.
               10  PO-DRVB0912-VSWE-VERSB-RV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB0912-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  PO-DRVB0912-VSWE-AV-BETRAG   PIC S9(09)V9(02) COMP-3.
      ****
      **** ENDE COPY DRVB0912 ****************************************
      ****
