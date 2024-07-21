      *****************************************************************
      *    COPY-MEMBER: WVVB0230                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0230 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    13.04.2018                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00583!13.04.18!2.1 ! NEU                       ! G. BRUDER *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0230                           *
      *****************************************************************
 
        05 WB-NI-VIVB0230-TAB.
           10 WB-NI-VIVB0230-ELE   PIC S9(04)  BINARY OCCURS  19.
        05 WB-NI-VIVB0230          REDEFINES   WB-NI-VIVB0230-TAB.
           10 WB-NI-FPRC-ST-NR            PIC S9(04)       BINARY.
           10 WB-NI-FPRC-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-FPRC-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-FPRC-LAST-UPDATE      PIC S9(04)       BINARY.
           10 WB-NI-FPRC-MS-NR            PIC S9(04)       BINARY.
           10 WB-NI-FPRC-12M-UMS-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-12M-EQF          PIC S9(04)       BINARY.
           10 WB-NI-FPRC-03M-UMS-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-EQF-ABZ-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-FAKTOR           PIC S9(04)       BINARY.
           10 WB-NI-FPRC-ANZ-MONATE       PIC S9(04)       BINARY.
           10 WB-NI-FPRC-12M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-03M-UMS-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-EQF-ABZ-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRC-EH-ABZ-NEG       PIC S9(04)       BINARY.
           10 WB-NI-FPRC-03M-KORR-FREI    PIC S9(04)       BINARY.
           10 WB-NI-FPRC-03M-KORR-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-FPRC-EH-ANT-FREI      PIC S9(04)       BINARY.
           10 WB-NI-FPRC-EH-ANT-UPFL      PIC S9(04)       BINARY.
 
        05 WB-VIVB0230.
           10 WB-FPRC-ST-NR               PIC S9(09)       BINARY.
           10 WB-FPRC-STICHTAG            PIC  X(10).
           10 WB-FPRC-ANSPR-VOM           PIC  X(10).
           10 WB-FPRC-LAST-UPDATE         PIC  X(26).
           10 WB-FPRC-MS-NR               PIC S9(04)       BINARY.
           10 WB-FPRC-12M-UMS-FREI        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-12M-EQF             PIC S9(03)V9(02) COMP-3.
           10 WB-FPRC-03M-UMS-FREI        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-EQF-ABZ-FREI        PIC S9(09)V9(02) COMP-3.
           10 WB-FPRC-FAKTOR              PIC S9(03)V9(02) COMP-3.
           10 WB-FPRC-ANZ-MONATE          PIC S9(04)       BINARY.
           10 WB-FPRC-12M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-03M-UMS-UPFL        PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-EQF-ABZ-UPFL        PIC S9(09)V9(02) COMP-3.
           10 WB-FPRC-EH-ABZ-NEG          PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-03M-KORR-FREI       PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-03M-KORR-UPFL       PIC S9(09)V9(01) COMP-3.
           10 WB-FPRC-EH-ANT-FREI         PIC S9(03)V9(02) COMP-3.
           10 WB-FPRC-EH-ANT-UPFL         PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  19                    *
      *****************************************************************

