      *****************************************************************
      *    COPY-MEMBER: WVVB0227                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0227 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G.BRUDER                                         *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G.BRUDER  *
200922* VLA-5363!20.09.22!2.2 ! Num. gepackte Felder      ! U. Heim   *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0227                           *
      *****************************************************************
 
        05 WB-NI-VIVB0227-TAB.
           10 WB-NI-VIVB0227-ELE   PIC S9(04)  BINARY OCCURS  11.
        05 WB-NI-VIVB0227          REDEFINES   WB-NI-VIVB0227-TAB.
           10 WB-NI-FPRM-BETR-ST-NR       PIC S9(04)       BINARY.
           10 WB-NI-FPRM-PART-ST-NR       PIC S9(04)       BINARY.
           10 WB-NI-FPRM-STICHTAG         PIC S9(04)       BINARY.
           10 WB-NI-KNR-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-KTY-ABZUG             PIC S9(04)       BINARY.
           10 WB-NI-FPRM-ANSPR-VOM        PIC S9(04)       BINARY.
           10 WB-NI-FPRM-BETR-UMSATZ      PIC S9(04)       BINARY.
           10 WB-NI-FPRM-BETR-UMS-MAX     PIC S9(04)       BINARY.
           10 WB-NI-FPRM-PART-UMSATZ      PIC S9(04)       BINARY.
           10 WB-NI-FPRM-PART-ABZUG       PIC S9(04)       BINARY.
           10 WB-NI-FPRM-LAST-UPDATE      PIC S9(04)       BINARY.
 
        05 WB-VIVB0227.
           10 WB-FPRM-BETR-ST-NR          PIC S9(09)       BINARY.
           10 WB-FPRM-PART-ST-NR          PIC S9(09)       BINARY.
           10 WB-FPRM-STICHTAG            PIC  X(10).
           10 WB-KNR-ABZUG                PIC S9(04)       BINARY.
           10 WB-KTY-ABZUG                PIC S9(04)       BINARY.
           10 WB-FPRM-ANSPR-VOM           PIC  X(10).
200922     10 WB-FPRM-BETR-UMSATZ         PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRM-BETR-UMS-MAX        PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRM-PART-UMSATZ         PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRM-PART-ABZUG          PIC S9(10)V9(01) COMP-3.
           10 WB-FPRM-LAST-UPDATE         PIC  X(26).
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *
      *****************************************************************
 

