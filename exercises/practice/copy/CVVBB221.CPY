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
      * DCLGEN TABLE(VIVBB221) AZNERF                                 *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBB221)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBB221 TABLE
CCC004*    ( AZNERF_PART_VBNR            INTEGER NOT NULL
CCC004*    , AZNERF_BETR_VBNR            INTEGER NOT NULL
CCC004*    , DIR_NR                      SMALLINT NOT NULL
CCC004*    , MS_NR                       SMALLINT NOT NULL
CCC004*    , AZNERF_ANSPR_VOM            DATE NOT NULL
CCC004*    , AZNERF_ANSPR_SEIT           DATE NOT NULL
CCC004*    , AZNERF_ANSPR_BIS            DATE
CCC004*    , AZNERF_EBENE                SMALLINT NOT NULL
CCC004*    , AZNERF_VERT_FAKTOR          SMALLINT NOT NULL
CCC004*    , AZNERF_BETR_FAKTOR          DECIMAL(11, 2) NOT NULL
CCC004*    , AZNERF_ANZAHL_MON           SMALLINT NOT NULL
CCC004*    , AZNERF_BETRAG               DECIMAL(11, 2) NOT NULL
CCC004*    , AZNERF_EH_GRENZE            DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EH_EIGEN             DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EH_EIGEN_MON         DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EQF_EIGEN            DECIMAL(05, 2) NOT NULL
CCC004*    , KNR_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_NERF_GRUND              SMALLINT NOT NULL
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL
CCC004*    , AZNERF_LAST_UPDATE          TIMESTAMP NOT NULL
CCC004*    , AZNERF_EG_UFREI             DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EG_UPFL              DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EG_UFREI_RECH        DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_EG_UPFL_RECH         DECIMAL(10, 1) NOT NULL
CCC004*    , AZNERF_BETRAG_UPFL          DECIMAL(11, 2) NOT NULL
CCC004*    , AZNERF_600_EH_ERF           CHAR(001) NOT NULL
CCC004*    , AZNERF_EH_NEU_EIGE          DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBB221                          *
      *****************************************************************

       01 VIVBB221-GESAMT.
          05 NI-VIVBB221-TAB.
             10 NI-VIVBB221-ELE    PIC S9(004) BINARY OCCURS  28.
          05 NI-VIVBB221           REDEFINES   NI-VIVBB221-TAB.
             10 NI-AZNERF-PART-VBNR      PIC S9(004) BINARY.
             10 NI-AZNERF-BETR-VBNR      PIC S9(004) BINARY.
             10 NI-DIR-NR                PIC S9(004) BINARY.
             10 NI-MS-NR                 PIC S9(004) BINARY.
             10 NI-AZNERF-ANSPR-VOM      PIC S9(004) BINARY.
             10 NI-AZNERF-ANSPR-SEIT     PIC S9(004) BINARY.
             10 NI-AZNERF-ANSPR-BIS      PIC S9(004) BINARY.
             10 NI-AZNERF-EBENE          PIC S9(004) BINARY.
             10 NI-AZNERF-VERT-FAKTOR    PIC S9(004) BINARY.
             10 NI-AZNERF-BETR-FAKTOR    PIC S9(004) BINARY.
             10 NI-AZNERF-ANZAHL-MON     PIC S9(004) BINARY.
             10 NI-AZNERF-BETRAG         PIC S9(004) BINARY.
             10 NI-AZNERF-EH-GRENZE      PIC S9(004) BINARY.
             10 NI-AZNERF-EH-EIGEN       PIC S9(004) BINARY.
             10 NI-AZNERF-EH-EIGEN-MON   PIC S9(004) BINARY.
             10 NI-AZNERF-EQF-EIGEN      PIC S9(004) BINARY.
             10 NI-KNR-NERF-GRUND        PIC S9(004) BINARY.
             10 NI-KTY-NERF-GRUND        PIC S9(004) BINARY.
             10 NI-KTY-WAEHRUNG          PIC S9(004) BINARY.
             10 NI-KNR-WAEHRUNG          PIC S9(004) BINARY.
             10 NI-AZNERF-LAST-UPDATE    PIC S9(004) BINARY.
             10 NI-AZNERF-EG-UFREI       PIC S9(004) BINARY.
             10 NI-AZNERF-EG-UPFL        PIC S9(004) BINARY.
             10 NI-AZNERF-EG-UFREI-RECH  PIC S9(004) BINARY.
             10 NI-AZNERF-EG-UPFL-RECH   PIC S9(004) BINARY.
             10 NI-AZNERF-BETRAG-UPFL    PIC S9(004) BINARY.
             10 NI-AZNERF-600-EH-ERF     PIC S9(004) BINARY.
             10 NI-AZNERF-EH-NEU-EIGEN   PIC S9(004) BINARY.

          05 DCL-VIVBB221.
             10 AZNERF-PART-VBNR         PIC S9(009) BINARY.
             10 AZNERF-BETR-VBNR         PIC S9(009) BINARY.
             10 DIR-NR                   PIC S9(004) BINARY.
             10 MS-NR                    PIC S9(004) BINARY.
             10 AZNERF-ANSPR-VOM         PIC  X(010).
             10 AZNERF-ANSPR-SEIT        PIC  X(010).
             10 AZNERF-ANSPR-BIS         PIC  X(010).
             10 AZNERF-EBENE             PIC S9(004) BINARY.
             10 AZNERF-VERT-FAKTOR       PIC S9(004) BINARY.
             10 AZNERF-BETR-FAKTOR       PIC S9(009)V9(002) COMP-3.
             10 AZNERF-ANZAHL-MON        PIC S9(004) BINARY.
             10 AZNERF-BETRAG            PIC S9(009)V9(002) COMP-3.
             10 AZNERF-EH-GRENZE         PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EH-EIGEN          PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EH-EIGEN-MON      PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EQF-EIGEN         PIC S9(003)V9(002) COMP-3.
             10 KNR-NERF-GRUND           PIC S9(004) BINARY.
             10 KTY-NERF-GRUND           PIC S9(004) BINARY.
             10 KTY-WAEHRUNG             PIC S9(004) BINARY.
             10 KNR-WAEHRUNG             PIC S9(004) BINARY.
             10 AZNERF-LAST-UPDATE       PIC  X(026).
             10 AZNERF-EG-UFREI          PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EG-UPFL           PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EG-UFREI-RECH     PIC S9(010)V9(001) COMP-3.
             10 AZNERF-EG-UPFL-RECH      PIC S9(010)V9(001) COMP-3.
             10 AZNERF-BETRAG-UPFL       PIC S9(009)V9(002) COMP-3.
             10 AZNERF-600-EH-ERF        PIC  X(001).
             10 AZNERF-EH-NEU-EIGEN      PIC S9(010)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  28                    *
      *****************************************************************
