      *****************************************************************
      *    COPY-MEMBER: DRVB0907                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0113                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    07.07.2008                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00309!07.07.08!2.1 ! NEU                       ! KLEEMEIER *
      * ASL00335!05.08.09!2.2 ! %-SÄTZE FÜR EH HINZU      ! KLEEMEIER *
      * ASL00498!29.10.13!2.3 ! BESTANDSSCHUTZ HINZU      ! BOLL     *
080722* VLA-5045!08.07.22!2.4 ! Num. gepackte Felder      ! HEIM      *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0907 - ERMITTELN OFFENER VSWP-SATZ            *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0907.
           05  PI-DRVB0907-DATEN.
               10  PI-DRVB0907-VSWP-LEISTUNG    PIC  X(001).
      *
       01  PO-DRVB0907.
           05  PO-DRVB0907-RC                   PIC  X(003).
               88  PO-DRVB0907-RC-OK                 VALUE 'OK '.
               88  PO-DRVB0907-RC-NOT-FOUND          VALUE 'NFD'.
           05  PO-DRVB0907-NI-VIVBB113.
               10  NI-DRVB0907-VSWP-LFDNR       PIC S9(04)       BINARY.
               10  NI-DRVB0907-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB0907-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-LEISTUNG    PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-BIS-STAT    PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-ART         PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EQF1        PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EQF2        PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EQF3        PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-EG-NEU   PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-GRP      PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-GES      PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-AUFST-BED   PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-VERTR-2007  PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-BES-ZUL     PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-40PROZ      PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-MS-NR       PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-VBP-NR      PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-STATUS      PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-LAST-UPDATE PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-EG-NEU-P PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-GRP-P    PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-EH-GES-P    PIC S9(04)       BINARY.
               10  NI-DRVB0907-VSWP-BS          PIC S9(04)       BINARY.
           05  PO-DRVB0907-DCL-VIVBB113.
               10  PO-DRVB0907-VSWP-LFDNR       PIC S9(04)       BINARY.
               10  PO-DRVB0907-KTY-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB0907-KNR-GESELL-TYP   PIC S9(04)       BINARY.
               10  PO-DRVB0907-VSWP-LEISTUNG    PIC  X(01).
               10  PO-DRVB0907-VSWP-BIS-STAT    PIC  X(10).
               10  PO-DRVB0907-VSWP-ART         PIC  X(01).
               10  PO-DRVB0907-VSWP-EQF1        PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0907-VSWP-EQF2        PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0907-VSWP-EQF3        PIC S9(03)V9(02) COMP-3.
080722         10  PO-DRVB0907-VSWP-EH-EG-NEU   PIC S9(10)V9(01) COMP-3.
080722         10  PO-DRVB0907-VSWP-EH-GRP      PIC S9(10)V9(01) COMP-3.
080722         10  PO-DRVB0907-VSWP-EH-GES      PIC S9(10)V9(01) COMP-3.
               10  PO-DRVB0907-VSWP-AUFST-BED   PIC S9(04)       BINARY.
               10  PO-DRVB0907-VSWP-VERTR-2007  PIC  X(01).
               10  PO-DRVB0907-VSWP-BES-ZUL     PIC  X(01).
               10  PO-DRVB0907-VSWP-40PROZ      PIC  X(01).
               10  PO-DRVB0907-VSWP-MS-NR       PIC S9(04)       BINARY.
               10  PO-DRVB0907-VSWP-VBP-NR      PIC S9(04)       BINARY.
               10  PO-DRVB0907-VSWP-STATUS      PIC  X(01).
               10  PO-DRVB0907-VSWP-LAST-UPDATE PIC  X(26).
               10  PO-DRVB0907-VSWP-EH-EG-NEU-P PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0907-VSWP-EH-GRP-P    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0907-VSWP-EH-GES-P    PIC S9(03)V9(02) COMP-3.
               10  PO-DRVB0907-VSWP-BS          PIC  X(01).
      ****
      **** ENDE COPY DRVB0907 ****************************************
      ****
