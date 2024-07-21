      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  29.07.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVB0235.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVB0235.CPv  $
      *
      *   Rev 2.1   Aug 27 2019 12:01:42   t13ae56
      *AVB01581: PGM - Paket 1 - Zentrale Ermittlungsroutine
      *
      *   Rev 2.0   Jul 29 2019 16:27:16   t13ae56
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    02.08.2019                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * VBL-819 !02.08.19!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0235) FAPE                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0235)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************
 
CCC005*    EXEC SQL DECLARE VIVB0235 TABLE
CCC004*    ( FAPE_LFDNR                  SMALLINT NOT NULL
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL
CCC004*    , FAPE_ART                    CHAR(01) NOT NULL
CCC004*    , MA_ST_NR                    INTEGER NOT NULL
CCC004*    , FAPE_ERM_KZ                 CHAR(02) NOT NULL
CCC004*    , FAPE_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , FAPE_MS_NR                  SMALLINT NOT NULL
CCC004*    , FAPE_VBP_NR                 SMALLINT NOT NULL
CCC004*    , FAPE_EH_EG_NEU              DECIMAL(10, 1)
CCC004*    , FAPE_EH_EG_GES              DECIMAL(10, 1)
CCC004*    , FAPE_EH_GES                 DECIMAL(10, 1)
CCC004*    , FAPE_EH_GES_KORR            DECIMAL(10, 1)
CCC004*    , FAPE_EH_GES_3MON            DECIMAL(10, 1)
CCC004*    , FAPE_JEQF_EG                DECIMAL(05, 2)
CCC004*    , FAPE_JEQF_GES               DECIMAL(05, 2)
CCC004*    , FAPE_BES                    SMALLINT
CCC004*    , FAPE_ALTERSGR_KZ            CHAR(01)
CCC004*    , FAPE_EINTR_KUEND            DATE
CCC004*    , FAPE_MAT_DAT_BIS            DATE
CCC004*    , FAPE_ERF_KZ_EG              CHAR(01) NOT NULL
CCC004*    , FAPE_ERF_KZ_GS              CHAR(01) NOT NULL
CCC004*    , FAPE_STUFE                  CHAR(01) NOT NULL
CCC004*    , FAPE_STUFE_DVAG             CHAR(01) NOT NULL
CCC003*    ) END-EXEC.
 
      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0235                          *
      *****************************************************************
 
       01  VIVB0235-GESAMT.
        05 NI-VIVB0235-TAB.
           10 NI-VIVB0235-ELE      PIC S9(04)  BINARY OCCURS  24.
        05 NI-VIVB0235             REDEFINES   NI-VIVB0235-TAB.
           10 NI-FAPE-LFDNR               PIC S9(04)       BINARY.
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.
           10 NI-FAPE-ART                 PIC S9(04)       BINARY.
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.
           10 NI-FAPE-ERM-KZ              PIC S9(04)       BINARY.
           10 NI-FAPE-LAST-UPDATE         PIC S9(04)       BINARY.
           10 NI-FAPE-MS-NR               PIC S9(04)       BINARY.
           10 NI-FAPE-VBP-NR              PIC S9(04)       BINARY.
           10 NI-FAPE-EH-EG-NEU           PIC S9(04)       BINARY.
           10 NI-FAPE-EH-EG-GES           PIC S9(04)       BINARY.
           10 NI-FAPE-EH-GES              PIC S9(04)       BINARY.
           10 NI-FAPE-EH-GES-KORR         PIC S9(04)       BINARY.
           10 NI-FAPE-EH-GES-3MON         PIC S9(04)       BINARY.
           10 NI-FAPE-JEQF-EG             PIC S9(04)       BINARY.
           10 NI-FAPE-JEQF-GES            PIC S9(04)       BINARY.
           10 NI-FAPE-BES                 PIC S9(04)       BINARY.
           10 NI-FAPE-ALTERSGR-KZ         PIC S9(04)       BINARY.
           10 NI-FAPE-EINTR-KUEND         PIC S9(04)       BINARY.
           10 NI-FAPE-MAT-DAT-BIS         PIC S9(04)       BINARY.
           10 NI-FAPE-ERF-KZ-EG           PIC S9(04)       BINARY.
           10 NI-FAPE-ERF-KZ-GS           PIC S9(04)       BINARY.
           10 NI-FAPE-STUFE               PIC S9(04)       BINARY.
           10 NI-FAPE-STUFE-DVAG          PIC S9(04)       BINARY.
 
        05 DCL-VIVB0235.
           10 FAPE-LFDNR                  PIC S9(04)       BINARY.
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.
           10 FAPE-ART                    PIC  X(01).
           10 MA-ST-NR                    PIC S9(09)       BINARY.
           10 FAPE-ERM-KZ                 PIC  X(02).
           10 FAPE-LAST-UPDATE            PIC  X(26).
           10 FAPE-MS-NR                  PIC S9(04)       BINARY.
           10 FAPE-VBP-NR                 PIC S9(04)       BINARY.
           10 FAPE-EH-EG-NEU              PIC S9(09)V9(01) COMP-3.
           10 FAPE-EH-EG-GES              PIC S9(09)V9(01) COMP-3.
           10 FAPE-EH-GES                 PIC S9(09)V9(01) COMP-3.
           10 FAPE-EH-GES-KORR            PIC S9(09)V9(01) COMP-3.
           10 FAPE-EH-GES-3MON            PIC S9(09)V9(01) COMP-3.
           10 FAPE-JEQF-EG                PIC S9(03)V9(02) COMP-3.
           10 FAPE-JEQF-GES               PIC S9(03)V9(02) COMP-3.
           10 FAPE-BES                    PIC S9(04)       BINARY.
           10 FAPE-ALTERSGR-KZ            PIC  X(01).
           10 FAPE-EINTR-KUEND            PIC  X(10).
           10 FAPE-MAT-DAT-BIS            PIC  X(10).
           10 FAPE-ERF-KZ-EG              PIC  X(01).
           10 FAPE-ERF-KZ-GS              PIC  X(01).
           10 FAPE-STUFE                  PIC  X(01).
           10 FAPE-STUFE-DVAG             PIC  X(01).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  24                    *
      *****************************************************************

