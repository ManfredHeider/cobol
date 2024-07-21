      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00544!07.11.01! 2.1! MS_40PROZ_REGEL entfällt, ! Walb     *
      *         !        !    ! MS_ABZPROZ_REGEL neu      !          *
261022* VLA-5571!26.10.22! 2.2! Num. gepackte Felder      ! Heim     *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0009)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0009))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0009 TABLE
CCC004*    ( MS_NR                          SMALLINT NOT NULL,
CCC004*      MS_EINSTIEG                    CHAR(1) NOT NULL,
CCC004*      MS_HPT                         CHAR(1) NOT NULL,
CCC004*      MS_34C                         CHAR(1) NOT NULL,
CCC004*      MS_ABZPROZ_REGEL               DECIMAL (5, 2) NOT NULL,
CCC004*      MS_EING_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_EIG_GES                  DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_EIG_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_GRU_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_EH_GES_PZTR                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_WEIT_EH_EIG                 DECIMAL(10, 1) NOT NULL,
CCC004*      MS_ZEITRAUM                    SMALLINT NOT NULL,
CCC004*      MS_EQF_EIG                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_EQF_GRU                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_EQF_GES                     DECIMAL(5, 2) NOT NULL,
CCC004*      MS_PROV_STF                    DECIMAL(5, 2) NOT NULL,
CCC004*      MS_GUELTIG_BIS                 DATE,
CCC004*      MS_BEZ_KURZ                    CHAR(4) NOT NULL,
CCC004*      MS_BEZ                         CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0009                           *
      ******************************************************************
       01  VIVB0009-GESAMT.
        05  NI-VIVB0009-TAB.
           10  NI-VIVB0009-ELE     PIC S9(04)  COMP   OCCURS  19 TIMES.
        05  NI-VIVB0009            REDEFINES   NI-VIVB0009-TAB.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-MS-EINSTIEG       PIC S9(04)  COMP.
           10 NI-MS-HPT            PIC S9(04)  COMP.
           10 NI-MS-34C            PIC S9(04)  COMP.
           10 NI-MS-ABZPROZ-REGEL  PIC S9(04)  COMP.
           10 NI-MS-EING-EH-EIG    PIC S9(04)  COMP.
           10 NI-MS-EH-EIG-GES     PIC S9(04)  COMP.
           10 NI-MS-EH-EIG-PZTR    PIC S9(04)  COMP.
           10 NI-MS-EH-GRU-PZTR    PIC S9(04)  COMP.
           10 NI-MS-EH-GES-PZTR    PIC S9(04)  COMP.
           10 NI-MS-WEIT-EH-EIG    PIC S9(04)  COMP.
           10 NI-MS-ZEITRAUM       PIC S9(04)  COMP.
           10 NI-MS-EQF-EIG        PIC S9(04)  COMP.
           10 NI-MS-EQF-GRU        PIC S9(04)  COMP.
           10 NI-MS-EQF-GES        PIC S9(04)  COMP.
           10 NI-MS-PROV-STF       PIC S9(04)  COMP.
           10 NI-MS-GUELTIG-BIS    PIC S9(04)  COMP.
           10 NI-MS-BEZ-KURZ       PIC S9(04)  COMP.
           10 NI-MS-BEZ            PIC S9(04)  COMP.
        05  DCL-VIVB0009.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 MS-EINSTIEG          PIC X(1).
           10 MS-HPT               PIC X(1).
           10 MS-34C               PIC X(1).
           10 MS-ABZPROZ-REGEL     PIC S9(3)V99 USAGE COMP-3.
261022     10 MS-EING-EH-EIG       PIC S9(10)V9 USAGE COMP-3.
261022     10 MS-EH-EIG-GES        PIC S9(10)V9 USAGE COMP-3.
261022     10 MS-EH-EIG-PZTR       PIC S9(10)V9 USAGE COMP-3.
261022     10 MS-EH-GRU-PZTR       PIC S9(10)V9 USAGE COMP-3.
261022     10 MS-EH-GES-PZTR       PIC S9(10)V9 USAGE COMP-3.
261022     10 MS-WEIT-EH-EIG       PIC S9(10)V9 USAGE COMP-3.
           10 MS-ZEITRAUM          PIC S9(4) USAGE COMP.
           10 MS-EQF-EIG           PIC S9(3)V99 USAGE COMP-3.
           10 MS-EQF-GRU           PIC S9(3)V99 USAGE COMP-3.
           10 MS-EQF-GES           PIC S9(3)V99 USAGE COMP-3.
           10 MS-PROV-STF          PIC S9(3)V99 USAGE COMP-3.
           10 MS-GUELTIG-BIS       PIC X(10).
           10 MS-BEZ-KURZ          PIC X(4).
           10 MS-BEZ               PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 19      *
      ******************************************************************
