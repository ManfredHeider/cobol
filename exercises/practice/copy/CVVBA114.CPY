      *****************************************************************
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    23.10.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00602!23.10.18!2.1 ! NEU                       ! G.BRUDER  *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA114) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA114)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVBA114 TABLE
CCC004*    ( VSWE_LFDNR                  SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , VSWE_LEISTUNG               CHAR(01) NOT NULL
CCC004*    , VSWE_ART                    CHAR(01) NOT NULL
CCC004*    , MA_ST_NR                    INTEGER NOT NULL
CCC004*    , VSWE_ERF_KZ                 CHAR(02) NOT NULL
CCC004*    , VSWE_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , VSWE_MS_NR                  SMALLINT NOT NULL
CCC004*    , VSWE_VBP_NR                 SMALLINT NOT NULL
CCC004*    , VSWE_EH_EG_NEU              DECIMAL(10, 1)
CCC004*    , VSWE_EH_EG_KORR             DECIMAL(10, 1)
CCC004*    , VSWE_EH_EG_GES              DECIMAL(10, 1)
CCC004*    , VSWE_EH_GRP                 DECIMAL(10, 1)
CCC004*    , VSWE_EH_GRP_KORR            DECIMAL(10, 1)
CCC004*    , VSWE_EH_GES                 DECIMAL(10, 1)
CCC004*    , VSWE_EH_GES_KORR            DECIMAL(10, 1)
CCC004*    , VSWE_JEQF_EG                DECIMAL(05, 2)
CCC004*    , VSWE_JEQF_GRP               DECIMAL(05, 2)
CCC004*    , VSWE_JEQF_GES               DECIMAL(05, 2)
CCC004*    , MAT_DAT_BIS                 DATE
CCC004*    , VSWE_BES                    SMALLINT
CCC004*    , VSWE_NEU_KZ                 CHAR(03) NOT NULL
CCC004*    , VSWE_VERTR_KZ               CHAR(01) NOT NULL
CCC004*    , VSWE_BES_ZUL_KZ             CHAR(01) NOT NULL
CCC004*    , VSWE_ALTERSGR_KZ            CHAR(01) NOT NULL
CCC004*    , VSWE_VERS_NR_LV             DECIMAL(15, 0)
CCC004*    , VSWE_VERSB_LV               DECIMAL(11, 2)
CCC004*    , VSWE_VERS_NR_RV             DECIMAL(15, 0)
CCC004*    , VSWE_VERSB_RV               DECIMAL(11, 2)
CCC004*    , VSWE_ANZ_ED                 SMALLINT
CCC004*    , VSWE_AV_BETRAG              DECIMAL(11, 2)
CCC004*    , VSWE_AV_BETR_ERM            DECIMAL(11, 2)
CCC004*    , VSWE_AV_BETR_BS             DECIMAL(11, 2)
CCC004*    , VSWE_AV_BETR_AG_GV          DECIMAL(11, 2)
CCC004*    , VSWE_AV_BETR_AG_WG          DECIMAL(11, 2)
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA114                           *
      *****************************************************************
 
       01  VIVBA114-GESAMT.
        05 NI-VIVBA114-TAB.
           10 NI-VIVBA114-ELE      PIC S9(04)  BINARY OCCURS  36.
        05 NI-VIVBA114             REDEFINES   NI-VIVBA114-TAB.
           10 NI-VSWE-LFDNR               PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-VSWE-LEISTUNG            PIC S9(04)       BINARY.
           10 NI-VSWE-ART                 PIC S9(04)       BINARY.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-VSWE-ERF-KZ              PIC S9(04)       BINARY.
           10 NI-VSWE-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-VSWE-MS-NR               PIC S9(04)       BINARY.
           10 NI-VSWE-VBP-NR              PIC S9(04)       BINARY.
           10 NI-VSWE-EH-EG-NEU           PIC S9(04)       BINARY.
           10 NI-VSWE-EH-EG-KORR          PIC S9(04)       BINARY.
           10 NI-VSWE-EH-EG-GES           PIC S9(04)       BINARY.
           10 NI-VSWE-EH-GRP              PIC S9(04)       BINARY.
           10 NI-VSWE-EH-GRP-KORR         PIC S9(04)       BINARY.
           10 NI-VSWE-EH-GES              PIC S9(04)       BINARY.
           10 NI-VSWE-EH-GES-KORR         PIC S9(04)       BINARY.
           10 NI-VSWE-JEQF-EG             PIC S9(04)       BINARY.
           10 NI-VSWE-JEQF-GRP            PIC S9(04)       BINARY.
           10 NI-VSWE-JEQF-GES            PIC S9(04)       BINARY.
           10 NI-MAT-DAT-BIS              PIC S9(04)       BINARY.
           10 NI-VSWE-BES                 PIC S9(04)       BINARY.
           10 NI-VSWE-NEU-KZ              PIC S9(04)       BINARY.
           10 NI-VSWE-VERTR-KZ            PIC S9(04)       BINARY.
           10 NI-VSWE-BES-ZUL-KZ          PIC S9(04)       BINARY.
           10 NI-VSWE-ALTERSGR-KZ         PIC S9(04)       BINARY.
           10 NI-VSWE-VERS-NR-LV          PIC S9(04)       BINARY.
           10 NI-VSWE-VERSB-LV            PIC S9(04)       BINARY.
           10 NI-VSWE-VERS-NR-RV          PIC S9(04)       BINARY.
           10 NI-VSWE-VERSB-RV            PIC S9(04)       BINARY.
           10 NI-VSWE-ANZ-ED              PIC S9(04)       BINARY.
           10 NI-VSWE-AV-BETRAG           PIC S9(04)       BINARY.
           10 NI-VSWE-AV-BETR-ERM         PIC S9(04)       BINARY.
           10 NI-VSWE-AV-BETR-BS          PIC S9(04)       BINARY.
           10 NI-VSWE-AV-BETR-AG-GV       PIC S9(04)       BINARY.
           10 NI-VSWE-AV-BETR-AG-WG       PIC S9(04)       BINARY.
 
        05 DCL-VIVBA114.
           10 VSWE-LFDNR                  PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 VSWE-LEISTUNG               PIC  X(01).
           10 VSWE-ART                    PIC  X(01).
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 VSWE-ERF-KZ                 PIC  X(02).
           10 VSWE-LAST-UPDATE            PIC  X(26).
           10 VSWE-MS-NR                  PIC S9(04)       BINARY.
           10 VSWE-VBP-NR                 PIC S9(04)       BINARY.
           10 VSWE-EH-EG-NEU              PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-EG-KORR             PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-EG-GES              PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-GRP                 PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-GRP-KORR            PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-GES                 PIC S9(09)V9(01) COMP-3.
           10 VSWE-EH-GES-KORR            PIC S9(09)V9(01) COMP-3.
           10 VSWE-JEQF-EG                PIC S9(03)V9(02) COMP-3.
           10 VSWE-JEQF-GRP               PIC S9(03)V9(02) COMP-3.
           10 VSWE-JEQF-GES               PIC S9(03)V9(02) COMP-3.
           10 MAT-DAT-BIS                 PIC  X(10).
           10 VSWE-BES                    PIC S9(04)       BINARY.
           10 VSWE-NEU-KZ                 PIC  X(03).
           10 VSWE-VERTR-KZ               PIC  X(01).
           10 VSWE-BES-ZUL-KZ             PIC  X(01).
           10 VSWE-ALTERSGR-KZ            PIC  X(01).
           10 VSWE-VERS-NR-LV             PIC S9(15)       COMP-3.
           10 VSWE-VERSB-LV               PIC S9(09)V9(02) COMP-3.
           10 VSWE-VERS-NR-RV             PIC S9(15)       COMP-3.
           10 VSWE-VERSB-RV               PIC S9(09)V9(02) COMP-3.
           10 VSWE-ANZ-ED                 PIC S9(04)       BINARY.
           10 VSWE-AV-BETRAG              PIC S9(09)V9(02) COMP-3.
           10 VSWE-AV-BETR-ERM            PIC S9(09)V9(02) COMP-3.
           10 VSWE-AV-BETR-BS             PIC S9(09)V9(02) COMP-3.
           10 VSWE-AV-BETR-AG-GV          PIC S9(09)V9(02) COMP-3.
           10 VSWE-AV-BETR-AG-WG          PIC S9(09)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  36                    *
      *****************************************************************

