      *****************************************************************
      *    ERSTELLT: Walb                                             *
      *    DATUM:    23.10.2013                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00498!23.10.13!2.1 ! NEU                       ! Walb      *
130722* VLA-5045!13.07.22!2.2 ! Num. gepackte Felder      ! HEIM      *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB113) VSWP                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB113)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB113 TABLE
CCC004*    ( VSWP_LFDNR                  SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , VSWP_LEISTUNG               CHAR(01) NOT NULL
CCC004*    , VSWP_BIS_STAT               DATE NOT NULL
CCC004*    , VSWP_ART                    CHAR(01) NOT NULL
CCC004*    , VSWP_EQF1                   DECIMAL(05, 2)
CCC004*    , VSWP_EQF2                   DECIMAL(05, 2)
CCC004*    , VSWP_EQF3                   DECIMAL(05, 2)
CCC004*    , VSWP_EH_EG_NEU              DECIMAL(10, 1)
CCC004*    , VSWP_EH_GRP                 DECIMAL(10, 1)
CCC004*    , VSWP_EH_GES                 DECIMAL(10, 1)
CCC004*    , VSWP_AUFST_BED              SMALLINT
CCC004*    , VSWP_VERTR_2007             CHAR(01)
CCC004*    , VSWP_BES_ZUL                CHAR(01)
CCC004*    , VSWP_40PROZ                 CHAR(01)
CCC004*    , VSWP_MS_NR                  SMALLINT NOT NULL
CCC004*    , VSWP_VBP_NR                 SMALLINT NOT NULL
CCC004*    , VSWP_STATUS                 CHAR(01) NOT NULL
CCC004*    , VSWP_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , VSWP_EH_EG_NEU_P            DECIMAL(05, 2)
CCC004*    , VSWP_EH_GRP_P               DECIMAL(05, 2)
CCC004*    , VSWP_EH_GES_P               DECIMAL(05, 2)
CCC004*    , VSWP_BS                     CHAR(01)
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBB113                           *
      *****************************************************************

       01  VIVBB113-GESAMT.
        05 NI-VIVBB113-TAB.
           10 NI-VIVBB113-ELE      PIC S9(04)  BINARY OCCURS  24.
        05 NI-VIVBB113             REDEFINES   NI-VIVBB113-TAB.
           10 NI-VSWP-LFDNR               PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-VSWP-LEISTUNG            PIC S9(04)       BINARY.
           10 NI-VSWP-BIS-STAT            PIC S9(04)       BINARY.
           10 NI-VSWP-ART                 PIC S9(04)       BINARY.
           10 NI-VSWP-EQF1                PIC S9(04)       BINARY.
           10 NI-VSWP-EQF2                PIC S9(04)       BINARY.
           10 NI-VSWP-EQF3                PIC S9(04)       BINARY.
           10 NI-VSWP-EH-EG-NEU           PIC S9(04)       BINARY.
           10 NI-VSWP-EH-GRP              PIC S9(04)       BINARY.
           10 NI-VSWP-EH-GES              PIC S9(04)       BINARY.
           10 NI-VSWP-AUFST-BED           PIC S9(04)       BINARY.
           10 NI-VSWP-VERTR-2007          PIC S9(04)       BINARY.
           10 NI-VSWP-BES-ZUL             PIC S9(04)       BINARY.
           10 NI-VSWP-40PROZ              PIC S9(04)       BINARY.
           10 NI-VSWP-MS-NR               PIC S9(04)       BINARY.
           10 NI-VSWP-VBP-NR              PIC S9(04)       BINARY.
           10 NI-VSWP-STATUS              PIC S9(04)       BINARY.
           10 NI-VSWP-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-VSWP-EH-EG-NEU-P         PIC S9(04)       BINARY.
           10 NI-VSWP-EH-GRP-P            PIC S9(04)       BINARY.
           10 NI-VSWP-EH-GES-P            PIC S9(04)       BINARY.
           10 NI-VSWP-BS                  PIC S9(04)       BINARY.

        05 DCL-VIVBB113.
           10 VSWP-LFDNR                  PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 VSWP-LEISTUNG               PIC  X(01).
           10 VSWP-BIS-STAT               PIC  X(10).
           10 VSWP-ART                    PIC  X(01).
           10 VSWP-EQF1                   PIC S9(03)V9(02) COMP-3.
           10 VSWP-EQF2                   PIC S9(03)V9(02) COMP-3.
           10 VSWP-EQF3                   PIC S9(03)V9(02) COMP-3.
130722     10 VSWP-EH-EG-NEU              PIC S9(10)V9(01) COMP-3.
130722     10 VSWP-EH-GRP                 PIC S9(10)V9(01) COMP-3.
130722     10 VSWP-EH-GES                 PIC S9(10)V9(01) COMP-3.
           10 VSWP-AUFST-BED              PIC S9(04)       BINARY.
           10 VSWP-VERTR-2007             PIC  X(01).
           10 VSWP-BES-ZUL                PIC  X(01).
           10 VSWP-40PROZ                 PIC  X(01).
           10 VSWP-MS-NR                  PIC S9(04)       BINARY.
           10 VSWP-VBP-NR                 PIC S9(04)       BINARY.
           10 VSWP-STATUS                 PIC  X(01).
           10 VSWP-LAST-UPDATE            PIC  X(26).
           10 VSWP-EH-EG-NEU-P            PIC S9(03)V9(02) COMP-3.
           10 VSWP-EH-GRP-P               PIC S9(03)V9(02) COMP-3.
           10 VSWP-EH-GES-P               PIC S9(03)V9(02) COMP-3.
           10 VSWP-BS                     PIC  X(01).

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  24                    *
      *****************************************************************
