      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA113.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA113.CPv  $
      *
      *   Rev 2.1   Aug 31 2009 09:33:48   t13ex43
      *ASL00335: PGM - VSW Release 1.1 (Dialog)
      *
      *   Rev 2.0   Aug 05 2009 09:57:16   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    05.08.2009                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00335!05.08.09!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA113) VSWP                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA113)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA113 TABLE
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
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA113                           *
      *****************************************************************

       01  VIVBA113-GESAMT.
        05 NI-VIVBA113-TAB.
           10 NI-VIVBA113-ELE      PIC S9(04)  BINARY OCCURS  23.
        05 NI-VIVBA113             REDEFINES   NI-VIVBA113-TAB.
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

        05 DCL-VIVBA113.
           10 VSWP-LFDNR                  PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 VSWP-LEISTUNG               PIC  X(01).
           10 VSWP-BIS-STAT               PIC  X(10).
           10 VSWP-ART                    PIC  X(01).
           10 VSWP-EQF1                   PIC S9(03)V9(02) COMP-3.
           10 VSWP-EQF2                   PIC S9(03)V9(02) COMP-3.
           10 VSWP-EQF3                   PIC S9(03)V9(02) COMP-3.
           10 VSWP-EH-EG-NEU              PIC S9(09)V9(01) COMP-3.
           10 VSWP-EH-GRP                 PIC S9(09)V9(01) COMP-3.
           10 VSWP-EH-GES                 PIC S9(09)V9(01) COMP-3.
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

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *
      *****************************************************************
