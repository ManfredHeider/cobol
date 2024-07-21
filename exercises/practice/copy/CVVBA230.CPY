      *****************************************************************
      *    ERSTELLT: GAERTTNER                                        *
      *    DATUM:    29.03.2023                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VBL-6478 |29.03.23| NEU                           | GAERTTNER *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVBA230) FPRC                                   *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA230)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVBA230 TABLE
CCC004*    ( FPRC_ST_NR                  INTEGER NOT NULL
CCC004*    , FPRC_STICHTAG               DATE NOT NULL
CCC004*    , FPRC_ANSPR_VOM              DATE NOT NULL
CCC004*    , FPRC_LAST_UPDATE            TIMESTAMP NOT NULL
CCC004*    , FPRC_MS_NR                  SMALLINT NOT NULL
CCC004*    , FPRC_12M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_12M_EQF                DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_03M_UMS_FREI           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EQF_ABZ_FREI           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRC_FAKTOR                 DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_ANZ_MONATE             SMALLINT NOT NULL
CCC004*    , FPRC_12M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_UMS_UPFL           DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EQF_ABZ_UPFL           DECIMAL(11, 2) NOT NULL
CCC004*    , FPRC_EH_ABZ_NEG             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_KORR_FREI          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_03M_KORR_UPFL          DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_ANT_FREI            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_EH_ANT_UPFL            DECIMAL(05, 2) NOT NULL
CCC004*    , FPRC_EH_GR_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_GR_FREI             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_GD_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_GD_FREI             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_ZV_UPFL             DECIMAL(10, 1) NOT NULL
CCC004*    , FPRC_EH_ZV_FREI             DECIMAL(10, 1) NOT NULL
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVBA230                          *
      *****************************************************************

       01 VIVBA230-GESAMT.
          05 NI-VIVBA230-TAB.
             10 NI-VIVBA230-ELE    PIC S9(004) BINARY OCCURS  25.
          05 NI-VIVBA230           REDEFINES   NI-VIVBA230-TAB.
             10 NI-FPRC-ST-NR            PIC S9(004) BINARY.
             10 NI-FPRC-STICHTAG         PIC S9(004) BINARY.
             10 NI-FPRC-ANSPR-VOM        PIC S9(004) BINARY.
             10 NI-FPRC-LAST-UPDATE      PIC S9(004) BINARY.
             10 NI-FPRC-MS-NR            PIC S9(004) BINARY.
             10 NI-FPRC-12M-UMS-FREI     PIC S9(004) BINARY.
             10 NI-FPRC-12M-EQF          PIC S9(004) BINARY.
             10 NI-FPRC-03M-UMS-FREI     PIC S9(004) BINARY.
             10 NI-FPRC-EQF-ABZ-FREI     PIC S9(004) BINARY.
             10 NI-FPRC-FAKTOR           PIC S9(004) BINARY.
             10 NI-FPRC-ANZ-MONATE       PIC S9(004) BINARY.
             10 NI-FPRC-12M-UMS-UPFL     PIC S9(004) BINARY.
             10 NI-FPRC-03M-UMS-UPFL     PIC S9(004) BINARY.
             10 NI-FPRC-EQF-ABZ-UPFL     PIC S9(004) BINARY.
             10 NI-FPRC-EH-ABZ-NEG       PIC S9(004) BINARY.
             10 NI-FPRC-03M-KORR-FREI    PIC S9(004) BINARY.
             10 NI-FPRC-03M-KORR-UPFL    PIC S9(004) BINARY.
             10 NI-FPRC-EH-ANT-FREI      PIC S9(004) BINARY.
             10 NI-FPRC-EH-ANT-UPFL      PIC S9(004) BINARY.
             10 NI-FPRC-EH-GR-UPFL       PIC S9(004) BINARY.
             10 NI-FPRC-EH-GR-FREI       PIC S9(004) BINARY.
             10 NI-FPRC-EH-GD-UPFL       PIC S9(004) BINARY.
             10 NI-FPRC-EH-GD-FREI       PIC S9(004) BINARY.
             10 NI-FPRC-EH-ZV-UPFL       PIC S9(004) BINARY.
             10 NI-FPRC-EH-ZV-FREI       PIC S9(004) BINARY.

          05 DCL-VIVBA230.
             10 FPRC-ST-NR               PIC S9(009) BINARY.
             10 FPRC-STICHTAG            PIC  X(010).
             10 FPRC-ANSPR-VOM           PIC  X(010).
             10 FPRC-LAST-UPDATE         PIC  X(026).
             10 FPRC-MS-NR               PIC S9(004) BINARY.
             10 FPRC-12M-UMS-FREI        PIC S9(009)V9(001) COMP-3.
             10 FPRC-12M-EQF             PIC S9(003)V9(002) COMP-3.
             10 FPRC-03M-UMS-FREI        PIC S9(009)V9(001) COMP-3.
             10 FPRC-EQF-ABZ-FREI        PIC S9(009)V9(002) COMP-3.
             10 FPRC-FAKTOR              PIC S9(003)V9(002) COMP-3.
             10 FPRC-ANZ-MONATE          PIC S9(004) BINARY.
             10 FPRC-12M-UMS-UPFL        PIC S9(009)V9(001) COMP-3.
             10 FPRC-03M-UMS-UPFL        PIC S9(009)V9(001) COMP-3.
             10 FPRC-EQF-ABZ-UPFL        PIC S9(009)V9(002) COMP-3.
             10 FPRC-EH-ABZ-NEG          PIC S9(009)V9(001) COMP-3.
             10 FPRC-03M-KORR-FREI       PIC S9(009)V9(001) COMP-3.
             10 FPRC-03M-KORR-UPFL       PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-ANT-FREI         PIC S9(003)V9(002) COMP-3.
             10 FPRC-EH-ANT-UPFL         PIC S9(003)V9(002) COMP-3.
             10 FPRC-EH-GR-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-GR-FREI          PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-GD-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-GD-FREI          PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-ZV-UPFL          PIC S9(009)V9(001) COMP-3.
             10 FPRC-EH-ZV-FREI          PIC S9(009)V9(001) COMP-3.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  25                    *
      *****************************************************************