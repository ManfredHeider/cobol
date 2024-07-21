      *****************************************************************
      *    COPY-MEMBER: DRVB1315                                      * 
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0114                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    01.07.2021                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG    ! DATUM  ! AENDERUNG                   ! NAME      *
      * -----------+--------+-----------------------------+---------- *
      * VLA-2905   !01.07.21! NEU                         ! KLEEMEIER *
      *            !        !                             !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB1315 - ERMITTELN VSWE-SATZ (A-VIEW)           * 
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB1315.
           05  PI-DRVB1315-DATEN.
               10  PI-DRVB1315-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  PI-DRVB1315-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PI-DRVB1315-VSWE-LEISTUNG    PIC  X(01).
               10  PI-DRVB1315-MA-ST-NR         PIC S9(09)       BINARY.
      *
       01  PO-DRVB1315.
         03  PO-DRVB1315-RC                     PIC  X(03).
               88  PO-DRVB1315-RC-OK                 VALUE 'OK '.
               88  PO-DRVB1315-RC-NOT-FOUND          VALUE 'NFD'.
         03  PO-DRVB1315-DATEN.
           05  PO-DRVB1315-NI-VIVBA114.
               10  NI-DRVB1315-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  NI-DRVB1315-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB1315-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-LEISTUNG    PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-ART         PIC S9(04)       BINARY.
               10  NI-DRVB1315-MA-ST-NR         PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-ERF-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-LAST-UPDATE PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-EG-NEU   PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-EG-KORR  PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-EG-GES   PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-GRP      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-GRP-KORR PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-GES      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-EH-GES-KORR PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-JEQF-EG     PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-JEQF-GRP    PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-JEQF-GES    PIC S9(04)       BINARY.
               10  NI-DRVB1315-MAT-DAT-BIS      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-BES         PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-NEU-KZ      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VERTR-KZ    PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-BES-ZUL-KZ  PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-ALTERSGR-KZ PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VERS-NR-LV  PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VERSB-LV    PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VERS-NR-RV  PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-VERSB-RV    PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-AV-BETRAG   PIC S9(04)       BINARY.
               10  NI-DRVB1315-VSWE-AV-BETR-ERM     PIC S9(04)   BINARY.
               10  NI-DRVB1315-VSWE-AV-BETR-BS      PIC S9(04)   BINARY.
               10  NI-DRVB1315-VSWE-AV-BETR-AG-GV   PIC S9(04)   BINARY.
               10  NI-DRVB1315-VSWE-AV-BETR-AG-WG   PIC S9(04)   BINARY.
           05  PO-DRVB1315-DCL-VIVBA114.
               10  PO-DRVB1315-VSWE-LFDNR       PIC S9(04)       BINARY.
               10  PO-DRVB1315-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB1315-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB1315-VSWE-LEISTUNG    PIC  X(01).
               10  PO-DRVB1315-VSWE-ART         PIC  X(01).
               10  PO-DRVB1315-MA-ST-NR         PIC S9(09)       BINARY.
               10  PO-DRVB1315-VSWE-ERF-KZ      PIC  X(02).
               10  PO-DRVB1315-VSWE-LAST-UPDATE PIC  X(26).
               10  PO-DRVB1315-VSWE-MS-NR       PIC S9(04)       BINARY.
               10  PO-DRVB1315-VSWE-VBP-NR      PIC S9(04)       BINARY.
               10  PO-DRVB1315-VSWE-EH-EG-NEU   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-EG-KORR  PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-EG-GES   PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-GRP      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-GRP-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-GES      PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-EH-GES-KORR PIC S9(09)V9(01) COMP-3.
               10  PO-DRVB1315-VSWE-JEQF-EG     PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-JEQF-GRP    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-JEQF-GES    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB1315-MAT-DAT-BIS      PIC  X(10).
               10  PO-DRVB1315-VSWE-BES         PIC S9(04)       BINARY.
               10  PO-DRVB1315-VSWE-NEU-KZ      PIC  X(03).
               10  PO-DRVB1315-VSWE-VERTR-KZ    PIC  X(01).
               10  PO-DRVB1315-VSWE-BES-ZUL-KZ  PIC  X(01).
               10  PO-DRVB1315-VSWE-ALTERSGR-KZ PIC  X(01).
               10  PO-DRVB1315-VSWE-VERS-NR-LV  PIC S9(15)       COMP-3.
               10  PO-DRVB1315-VSWE-VERSB-LV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-VERS-NR-RV  PIC S9(15)       COMP-3.
               10  PO-DRVB1315-VSWE-VERSB-RV    PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-ANZ-ED      PIC S9(04)       BINARY.
               10  PO-DRVB1315-VSWE-AV-BETRAG   PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-AV-BETR-ERM PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-AV-BETR-BS  PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-AV-BETR-AG-GV
                                                PIC S9(09)V9(02) COMP-3.
               10  PO-DRVB1315-VSWE-AV-BETR-AG-WG
                                                PIC S9(09)V9(02) COMP-3.
      ****
      **** ENDE COPY DRVB1315 ****************************************  
      ****