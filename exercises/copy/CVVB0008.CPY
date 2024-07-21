      ******************************************************************
      * DCLGEN TABLE(VIVB0008)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0008))              *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
261022* VLA-5571!26.10.22! 2.0! Num. gepackte Felder      ! Heim     *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
CCC005*    EXEC SQL DECLARE VIVB0008 TABLE
CCC004*    ( MS_NR                          SMALLINT NOT NULL,
CCC004*      BS_NR                          SMALLINT NOT NULL,
CCC004*      MSBS_EH_EIG_GES                DECIMAL(10, 1) NOT NULL,
CCC004*      MSBS_EH_EIG_PZTR               DECIMAL(10, 1) NOT NULL,
CCC004*      MSBS_ZEITRAUM                  SMALLINT NOT NULL,
CCC004*      MSBS_EQF_EIG                   DECIMAL(5, 2) NOT NULL,
CCC004*      MSBS_PROV_STF                  DECIMAL(5, 2) NOT NULL,
CCC004*      MSBS_LAUFZEIT                  SMALLINT NOT NULL,
CCC004*      MSBS_GUELTIG_BIS               DATE,
CCC004*      MSBS_BEZ                       CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0008                           *
      ******************************************************************
       01  VIVB0008-GESAMT.
        05  NI-VIVB0008-TAB.
           10  NI-VIVB0008-ELE     PIC S9(04)  COMP   OCCURS  10 TIMES.
        05  NI-VIVB0008            REDEFINES   NI-VIVB0008-TAB.
           10 NI-MS-NR             PIC S9(04)  COMP.
           10 NI-BS-NR             PIC S9(04)  COMP.
           10 NI-MSBS-EH-EIG-GES   PIC S9(04)  COMP.
           10 NI-MSBS-EH-EIG-PZTR  PIC S9(04)  COMP.
           10 NI-MSBS-ZEITRAUM     PIC S9(04)  COMP.
           10 NI-MSBS-EQF-EIG      PIC S9(04)  COMP.
           10 NI-MSBS-PROV-STF     PIC S9(04)  COMP.
           10 NI-MSBS-LAUFZEIT     PIC S9(04)  COMP.
           10 NI-MSBS-GUELTIG-BIS  PIC S9(04)  COMP.
           10 NI-MSBS-BEZ          PIC S9(04)  COMP.
        05  DCL-VIVB0008.
           10 MS-NR                PIC S9(4)    USAGE COMP.
           10 BS-NR                PIC S9(4)    USAGE COMP.
261022     10 MSBS-EH-EIG-GES      PIC S9(10)V9 USAGE COMP-3.
261022     10 MSBS-EH-EIG-PZTR     PIC S9(10)V9 USAGE COMP-3.
           10 MSBS-ZEITRAUM        PIC S9(4)    USAGE COMP.
           10 MSBS-EQF-EIG         PIC S9(3)V99 USAGE COMP-3.
           10 MSBS-PROV-STF        PIC S9(3)V99 USAGE COMP-3.
           10 MSBS-LAUFZEIT        PIC S9(4) USAGE COMP.
           10 MSBS-GUELTIG-BIS     PIC X(10).
           10 MSBS-BEZ             PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *
      ******************************************************************
