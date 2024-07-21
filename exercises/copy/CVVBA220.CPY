      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.07.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   CVVBA220.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/CVVBA220.CPv  $
      *
      *   Rev 2.1   Aug 10 2017 16:19:02   t13ae13
      *ASL00567: LUCY2 für Ausbilderbonus - SL12 (swl)
      *
      *   Rev 2.0   Jul 05 2017 12:07:28   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    ERSTELLT: WALB                                             *
      *    DATUM:    05.07.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+---------- *
      * ASL00567!05.07.17!2.1 ! NEU                       ! WALB      *
      *         !        !    !                           !           *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA220) ALIAS                               *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA220)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA220 TABLE
CCC004*    ( AZERF_PART_VBNR             INTEGER NOT NULL
CCC004*    , AZERF_BETR_VBNR             INTEGER NOT NULL
CCC004*    , DIR_NR                      SMALLINT NOT NULL
CCC004*    , MS_NR                       SMALLINT NOT NULL
CCC004*    , AZERF_ANSPR_VOM             DATE NOT NULL
CCC004*    , AZERF_ANSPR_SEIT            DATE NOT NULL
CCC004*    , AZERF_ANSPR_BIS             DATE
CCC004*    , AZERF_EBENE                 SMALLINT NOT NULL
CCC004*    , AZERF_VERT_FAKTOR           SMALLINT NOT NULL
CCC004*    , AZERF_BETR_FAKTOR           DECIMAL(11, 2) NOT NULL
CCC004*    , AZERF_ANZAHL_MON            SMALLINT NOT NULL
CCC004*    , AZERF_BETRAG                DECIMAL(11, 2) NOT NULL
CCC004*    , AZERF_EH_GRENZE             DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EH_EIGEN              DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EH_EIGEN_MON          DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EQF_EIGEN             DECIMAL(05, 2) NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , AZERF_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , AZERF_EG_UFREI              DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EG_UPFL               DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EG_UFREI_RECH         DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_EG_UPFL_RECH          DECIMAL(10, 1) NOT NULL
CCC004*    , AZERF_BETRAG_UPFL           DECIMAL(11, 2) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVBA220                           *
      *****************************************************************

       01  VIVBA220-GESAMT.
        05 NI-VIVBA220-TAB.
           10 NI-VIVBA220-ELE      PIC S9(04)  BINARY OCCURS  24.
        05 NI-VIVBA220             REDEFINES   NI-VIVBA220-TAB.
           10 NI-AZERF-PART-VBNR          PIC S9(04)       BINARY.
           10 NI-AZERF-BETR-VBNR          PIC S9(04)       BINARY.
           10 NI-DIR-NR                   PIC S9(04)       BINARY.
           10 NI-MS-NR                    PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-VOM          PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-SEIT         PIC S9(04)       BINARY.
           10 NI-AZERF-ANSPR-BIS          PIC S9(04)       BINARY.
           10 NI-AZERF-EBENE              PIC S9(04)       BINARY.
           10 NI-AZERF-VERT-FAKTOR        PIC S9(04)       BINARY.
           10 NI-AZERF-BETR-FAKTOR        PIC S9(04)       BINARY.
           10 NI-AZERF-ANZAHL-MON         PIC S9(04)       BINARY.
           10 NI-AZERF-BETRAG             PIC S9(04)       BINARY.
           10 NI-AZERF-EH-GRENZE          PIC S9(04)       BINARY.
           10 NI-AZERF-EH-EIGEN           PIC S9(04)       BINARY.
           10 NI-AZERF-EH-EIGEN-MON       PIC S9(04)       BINARY.
           10 NI-AZERF-EQF-EIGEN          PIC S9(04)       BINARY.
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.
           10 NI-AZERF-LAST-UPDATE        PIC S9(04)       BINARY.
           10 NI-AZERF-EG-UFREI           PIC S9(04)       BINARY.
           10 NI-AZERF-EG-UPFL            PIC S9(04)       BINARY.
           10 NI-AZERF-EG-UFREI-RECH      PIC S9(04)       BINARY.
           10 NI-AZERF-EG-UPFL-RECH       PIC S9(04)       BINARY.
           10 NI-AZERF-BETRAG-UPFL        PIC S9(04)       BINARY.

        05 DCL-VIVBA220.
           10 AZERF-PART-VBNR             PIC S9(09)       BINARY.
           10 AZERF-BETR-VBNR             PIC S9(09)       BINARY.
           10 DIR-NR                      PIC S9(04)       BINARY.
           10 MS-NR                       PIC S9(04)       BINARY.
           10 AZERF-ANSPR-VOM             PIC  X(10).
           10 AZERF-ANSPR-SEIT            PIC  X(10).
           10 AZERF-ANSPR-BIS             PIC  X(10).
           10 AZERF-EBENE                 PIC S9(04)       BINARY.
           10 AZERF-VERT-FAKTOR           PIC S9(04)       BINARY.
           10 AZERF-BETR-FAKTOR           PIC S9(09)V9(02) COMP-3.
           10 AZERF-ANZAHL-MON            PIC S9(04)       BINARY.
           10 AZERF-BETRAG                PIC S9(09)V9(02) COMP-3.
           10 AZERF-EH-GRENZE             PIC S9(09)V9(01) COMP-3.
           10 AZERF-EH-EIGEN              PIC S9(09)V9(01) COMP-3.
           10 AZERF-EH-EIGEN-MON          PIC S9(09)V9(01) COMP-3.
           10 AZERF-EQF-EIGEN             PIC S9(03)V9(02) COMP-3.
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.
           10 AZERF-LAST-UPDATE           PIC  X(26).
           10 AZERF-EG-UFREI              PIC S9(09)V9(01) COMP-3.
           10 AZERF-EG-UPFL               PIC S9(09)V9(01) COMP-3.
           10 AZERF-EG-UFREI-RECH         PIC S9(09)V9(01) COMP-3.
           10 AZERF-EG-UPFL-RECH          PIC S9(09)V9(01) COMP-3.
           10 AZERF-BETRAG-UPFL           PIC S9(09)V9(02) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  24                    *
      *****************************************************************

