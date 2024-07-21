      *****************************************************************
      *    COPY-MEMBER: WVVB0229                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0229 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0229                           *
      *****************************************************************
 
        05 WB-NI-VIVB0229-TAB.
           10 WB-NI-VIVB0229-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVB0229          REDEFINES   WB-NI-VIVB0229-TAB.
           10 WB-NI-FPRB-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-FPRB-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-FPRB-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-FPRB-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-FPRB-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-FPRB-12M-UMS-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-12M-EQF          PIC S9(04)       BINARY.
           10 WB-NI-FPRB-03M-UMS-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-EQF-ABZ-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-FPRB-ANZ-MONATE       PIC S9(04)       BINARY.
           10 WB-NI-FPRB-12M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-03M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-EQF-ABZ-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRB-EH-ABZ-NEG       PIC S9(04)       BINARY.
           10 WB-NI-FPRB-03M-KORR-FREI    PIC S9(04)       BINARY.
           10 WB-NI-FPRB-03M-KORR-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-FPRB-EH-ANT-FREI      PIC S9(04)       BINARY.
           10 WB-NI-FPRB-EH-ANT-UPFL      PIC S9(04)       BINARY.
 
        05 WB-VIVB0229.
           10 WB-FPRB-ST-NR               PIC S9(09)       BINARY.
           10 WB-FPRB-STICHTAG            PIC  X(10).
           10 WB-FPRB-ANSPR-VOM           PIC  X(10).
           10 WB-FPRB-LAST-UPDATE         PIC  X(26).
           10 WB-FPRB-MS-NR               PIC S9(04)       BINARY.
           10 WB-FPRB-12M-UMS-FREI        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-12M-EQF             PIC S9(03)V9(02) COMP-3.
           10 WB-FPRB-03M-UMS-FREI        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-EQF-ABZ-FREI        PIC S9(09)V9(02) COMP-3.
           10 WB-FPRB-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-FPRB-ANZ-MONATE          PIC S9(04)       BINARY.
           10 WB-FPRB-12M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-03M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-EQF-ABZ-UPFL        PIC S9(09)V9(02) COMP-3.
           10 WB-FPRB-EH-ABZ-NEG          PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-03M-KORR-FREI       PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-03M-KORR-UPFL       PIC S9(09)V9(01) COMP-3.
           10 WB-FPRB-EH-ANT-FREI         PIC S9(03)V9(02) COMP-3.
           10 WB-FPRB-EH-ANT-UPFL         PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

