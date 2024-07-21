      *****************************************************************
      *    ERSTELLT: THURMANN                                         *
      *    DATUM:    24.08.2022                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * JIRA     | DATUM  | AENDERUNG                     | NAME      *
      * ---------+--------+-------------------------------+---------- *
      * VLA-4870 |24.08.22| NEU                           | THURMANN  *
      *          |        |                               |           *
      *          |        |                               |           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * DCLGEN TABLE(VIVB0356) WTAUS                                  *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0356)              *
      *        ACTION(REPLACE)                                        *
      *        QUOTE                                                  *
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *
      *****************************************************************

CCC005*    EXEC SQL DECLARE VIVB0356 TABLE
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL
CCC004*    , WTAUS_ANSPR_VOM             DATE NOT NULL
CCC004*    , KNR_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , KTY_ANSPR_GRUND             SMALLINT NOT NULL
CCC004*    , WTAUS_LAST_UPDATE           TIMESTAMP NOT NULL
CCC004*    , WTAUS_ANSPR_BETRAG          DECIMAL(11, 2) NOT NULL
CCC004*    , KTY_ANSPR_STATUS            SMALLINT
CCC004*    , KNR_ANSPR_STATUS            SMALLINT
CCC003*    ) END-EXEC.

      *****************************************************************
      * COBOL DEKLARATION FUER VIEW VIVB0356                          *
      *****************************************************************

       01 VIVB0356-GESAMT.
          05 NI-VIVB0356-TAB.
             10 NI-VIVB0356-ELE    PIC S9(004) BINARY OCCURS   8.
          05 NI-VIVB0356           REDEFINES   NI-VIVB0356-TAB.
             10 NI-MA-ST-NR              PIC S9(004) BINARY.
             10 NI-WTAUS-ANSPR-VOM       PIC S9(004) BINARY.
             10 NI-KNR-ANSPR-GRUND       PIC S9(004) BINARY.
             10 NI-KTY-ANSPR-GRUND       PIC S9(004) BINARY.
             10 NI-WTAUS-LAST-UPDATE     PIC S9(004) BINARY.
             10 NI-WTAUS-ANSPR-BETRAG    PIC S9(004) BINARY.
             10 NI-KTY-ANSPR-STATUS      PIC S9(004) BINARY.
             10 NI-KNR-ANSPR-STATUS      PIC S9(004) BINARY.

          05 DCL-VIVB0356.
             10 MA-ST-NR                 PIC S9(009) BINARY.
             10 WTAUS-ANSPR-VOM          PIC  X(010).
             10 KNR-ANSPR-GRUND          PIC S9(004) BINARY.
             10 KTY-ANSPR-GRUND          PIC S9(004) BINARY.
             10 WTAUS-LAST-UPDATE        PIC  X(026).
             10 WTAUS-ANSPR-BETRAG       PIC S9(009)V9(002) COMP-3.
             10 KTY-ANSPR-STATUS         PIC S9(004) BINARY.
             10 KNR-ANSPR-STATUS         PIC S9(004) BINARY.

      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   8                    *
      *****************************************************************
