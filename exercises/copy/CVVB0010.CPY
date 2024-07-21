      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
261022* VLA-5571!26.10.22! 2.0! Num. gepackte Felder      ! Heim     *
      *         !        !    !                           !          *
      *                                                              *
      ****************************************************************
      ******************************************************************
      * DCLGEN TABLE(VIVB0010)                                         *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0010))              *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0010 TABLE
CCC004*    ( VBP_NR                         SMALLINT NOT NULL,
CCC004*      VBP_EH_EIG_PZTR                DECIMAL(10, 1) NOT NULL,
CCC004*      VBP_EH_EIG_GES                 DECIMAL(10, 1) NOT NULL,
CCC004*      VBP_WEIT_EH_EIG                DECIMAL(10, 1) NOT NULL,
CCC004*      VBP_ZEITRAUM                   SMALLINT NOT NULL,
CCC004*      VBP_EQF_EIG                    DECIMAL(5, 2) NOT NULL,
CCC004*      VBP_PROV_STF                   DECIMAL(5, 2) NOT NULL,
CCC004*      VBP_AUFSTIEG                   SMALLINT NOT NULL,
CCC004*      VBP_GUELTIG_BIS                DATE,
CCC004*      VBP_BEZ_KURZ                   CHAR(4) NOT NULL,
CCC004*      VBP_BEZ                        CHAR(40) NOT NULL
CCC003*    ) END-EXEC.
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0010                           *
      ******************************************************************
       01  VIVB0010-GESAMT.
        05  NI-VIVB0010-TAB.
           10  NI-VIVB0010-ELE     PIC S9(04)  COMP   OCCURS  11 TIMES.
        05  NI-VIVB0010            REDEFINES   NI-VIVB0010-TAB.
           10 NI-VBP-NR            PIC S9(04)  COMP.
           10 NI-VBP-EH-EIG-PZTR   PIC S9(04)  COMP.
           10 NI-VBP-EH-EIG-GES    PIC S9(04)  COMP.
           10 NI-VBP-WEIT-EH-EIG   PIC S9(04)  COMP.
           10 NI-VBP-ZEITRAUM      PIC S9(04)  COMP.
           10 NI-VBP-EQF-EIG       PIC S9(04)  COMP.
           10 NI-VBP-PROV-STF      PIC S9(04)  COMP.
           10 NI-VBP-AUFSTIEG      PIC S9(04)  COMP.
           10 NI-VBP-GUELTIG-BIS   PIC S9(04)  COMP.
           10 NI-VBP-BEZ-KURZ      PIC S9(04)  COMP.
           10 NI-VBP-BEZ           PIC S9(04)  COMP.
        05  DCL-VIVB0010.
           10 VBP-NR               PIC S9(4)    USAGE COMP.
261022     10 VBP-EH-EIG-PZTR      PIC S9(10)V9 USAGE COMP-3.
261022     10 VBP-EH-EIG-GES       PIC S9(10)V9 USAGE COMP-3.
261022     10 VBP-WEIT-EH-EIG      PIC S9(10)V9 USAGE COMP-3.
           10 VBP-ZEITRAUM         PIC S9(4)    USAGE COMP.
           10 VBP-EQF-EIG          PIC S999V99  USAGE COMP-3.
           10 VBP-PROV-STF         PIC S999V99  USAGE COMP-3.
           10 VBP-AUFSTIEG         PIC S9(4)    USAGE COMP.
           10 VBP-GUELTIG-BIS      PIC X(10).
           10 VBP-BEZ-KURZ         PIC X(4).
           10 VBP-BEZ              PIC X(40).
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *
      ******************************************************************
