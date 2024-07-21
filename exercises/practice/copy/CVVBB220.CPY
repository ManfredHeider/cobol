      *****************************************************************
      *    ERSTELLT: -                                                *
      *    DATUM:    04.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-5181 !04.08.22! NEU                           ! U. HEIM   *
      *          !        !                               !           *
      *          !        !                               !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBB220) AZERF                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB220)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB220 TABLE
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
CCC004*    , AZERF_600_EH_ERF            CHAR(001) NOT NULL
CCC004*    , AZERF_EH_NEU_EIGEN          DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBB220                          *
      *****************************************************************

       01 VIVBB220-GESAMT.
          05 NI-VIVBB220-TAB.
             10 NI-VIVBB220-ELE    PIC S9(004) BINARY OCCURS  26.
          05 NI-VIVBB220           REDEFINES   NI-VIVBB220-TAB.
             10 NI-AZERF-PART-VBNR       PIC S9(004) BINARY.
             10 NI-AZERF-BETR-VBNR       PIC S9(004) BINARY.
             10 NI-DIR-NR                PIC S9(004) BINARY.
             10 NI-MS-NR                 PIC S9(004) BINARY.
             10 NI-AZERF-ANSPR-VOM       PIC S9(004) BINARY.
             10 NI-AZERF-ANSPR-SEIT      PIC S9(004) BINARY.
             10 NI-AZERF-ANSPR-BIS       PIC S9(004) BINARY.
             10 NI-AZERF-EBENE           PIC S9(004) BINARY.
             10 NI-AZERF-VERT-FAKTOR     PIC S9(004) BINARY.
             10 NI-AZERF-BETR-FAKTOR     PIC S9(004) BINARY.
             10 NI-AZERF-ANZAHL-MON      PIC S9(004) BINARY.
             10 NI-AZERF-BETRAG          PIC S9(004) BINARY.
             10 NI-AZERF-EH-GRENZE       PIC S9(004) BINARY.
             10 NI-AZERF-EH-EIGEN        PIC S9(004) BINARY.
             10 NI-AZERF-EH-EIGEN-MON    PIC S9(004) BINARY.
             10 NI-AZERF-EQF-EIGEN       PIC S9(004) BINARY.
             10 NI-KTY-WAEHRUNG          PIC S9(004) BINARY.
             10 NI-KNR-WAEHRUNG          PIC S9(004) BINARY.
             10 NI-AZERF-LAST-UPDATE     PIC S9(004) BINARY.
             10 NI-AZERF-EG-UFREI        PIC S9(004) BINARY.
             10 NI-AZERF-EG-UPFL         PIC S9(004) BINARY.
             10 NI-AZERF-EG-UFREI-RECH   PIC S9(004) BINARY.
             10 NI-AZERF-EG-UPFL-RECH    PIC S9(004) BINARY.
             10 NI-AZERF-BETRAG-UPFL     PIC S9(004) BINARY.
             10 NI-AZERF-600-EH-ERF      PIC S9(004) BINARY.
             10 NI-AZERF-EH-NEU-EIGEN    PIC S9(004) BINARY.

          05 DCL-VIVBB220.
             10 AZERF-PART-VBNR          PIC S9(009) BINARY.
             10 AZERF-BETR-VBNR          PIC S9(009) BINARY.
             10 DIR-NR                   PIC S9(004) BINARY.
             10 MS-NR                    PIC S9(004) BINARY.
             10 AZERF-ANSPR-VOM          PIC  X(010).
             10 AZERF-ANSPR-SEIT         PIC  X(010).
             10 AZERF-ANSPR-BIS          PIC  X(010).
             10 AZERF-EBENE              PIC S9(004) BINARY.
             10 AZERF-VERT-FAKTOR        PIC S9(004) BINARY.
             10 AZERF-BETR-FAKTOR        PIC S9(009)V9(002) COMP-3.
             10 AZERF-ANZAHL-MON         PIC S9(004) BINARY.
             10 AZERF-BETRAG             PIC S9(009)V9(002) COMP-3.
             10 AZERF-EH-GRENZE          PIC S9(010)V9(001) COMP-3.
             10 AZERF-EH-EIGEN           PIC S9(010)V9(001) COMP-3.
             10 AZERF-EH-EIGEN-MON       PIC S9(010)V9(001) COMP-3.
             10 AZERF-EQF-EIGEN          PIC S9(003)V9(002) COMP-3.
             10 KTY-WAEHRUNG             PIC S9(004) BINARY.
             10 KNR-WAEHRUNG             PIC S9(004) BINARY.
             10 AZERF-LAST-UPDATE        PIC  X(026).
             10 AZERF-EG-UFREI           PIC S9(010)V9(001) COMP-3.
             10 AZERF-EG-UPFL            PIC S9(010)V9(001) COMP-3.
             10 AZERF-EG-UFREI-RECH      PIC S9(010)V9(001) COMP-3.
             10 AZERF-EG-UPFL-RECH       PIC S9(010)V9(001) COMP-3.
             10 AZERF-BETRAG-UPFL        PIC S9(009)V9(002) COMP-3.
             10 AZERF-600-EH-ERF         PIC  X(001).
             10 AZERF-EH-NEU-EIGEN       PIC S9(010)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  26                    *
      *****************************************************************
