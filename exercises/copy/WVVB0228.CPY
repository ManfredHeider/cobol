      *****************************************************************
      *    COPY-MEMBER: WVVB0228                                      *
      *    --------------------------------------------------------   *
      *    WORKBEREICH FÜR VIVB0228 (ALIAS)                        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: G. BRUDER                                        *
      *    DATUM:    02.05.2017                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00569!02.05.17!2.1 ! NEU                       ! G. BRUDER *
200922* VLA-5363!20.09.22!2.2 ! Num. gepackte Felder      ! U. Heim   *
      *         !        !    !                           !           *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
      * COBOL DEKLARATION FÜR VIEW VIVB0228                           *
      *****************************************************************
 
        05 WB-NI-VIVB0228-TAB.
           10 WB-NI-VIVB0228-ELE   PIC S9(04)  BINARY OCCURS  31.
        05 WB-NI-VIVB0228          REDEFINES   WB-NI-VIVB0228-TAB.
           10 WB-NI-FPRDD-ST-NR           PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-STICHTAG        PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ANSPR-MONAT     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ANSPR-VOM       PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-LAST-UPDATE     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-MS-NR           PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-12M-UMS-FREI    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-12M-EQF         PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-03M-UMS-FREI    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-PRO-ABZUG-FR    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-N-PRO-ABZ-FR    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ZW-BETRAG       PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-EQF-ABZUG-FR    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-BETRAG-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-FAKTOR          PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ANZ-MONATE      PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-UMS-BED-ERF     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-S-AUSZAHLUNG    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-S-NACH-DIR      PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-12M-UMS-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-03M-UMS-UPFL    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-PRO-ABZUG-UP    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-N-PRO-ABZ-UP    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ZW-BETRAG-UP    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-EQF-ABZUG-UP    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-BETRAG-UPFL     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ABZUG-NEG       PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-03M-KORR-FRE    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-03M-KORR-UPF    PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ANTEIL-FREI     PIC S9(04)       BINARY.
           10 WB-NI-FPRDD-ANTEIL-UPFL     PIC S9(04)       BINARY.
 
        05 WB-VIVB0228.
           10 WB-FPRDD-ST-NR              PIC S9(09)       BINARY.
           10 WB-FPRDD-STICHTAG           PIC  X(10).
           10 WB-FPRDD-ANSPR-MONAT        PIC  X(10).
           10 WB-FPRDD-ANSPR-VOM          PIC  X(10).
           10 WB-FPRDD-LAST-UPDATE        PIC  X(26).
           10 WB-FPRDD-MS-NR              PIC S9(04)       BINARY.
200922     10 WB-FPRDD-12M-UMS-FREI       PIC S9(10)V9(01) COMP-3.
           10 WB-FPRDD-12M-EQF            PIC S9(03)V9(02) COMP-3.
200922     10 WB-FPRDD-03M-UMS-FREI       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-PRO-ABZUG-FR       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-N-PRO-ABZ-FR       PIC S9(10)V9(01) COMP-3.
           10 WB-FPRDD-ZW-BETRAG          PIC S9(09)V9(02) COMP-3.
           10 WB-FPRDD-EQF-ABZUG-FR       PIC S9(09)V9(02) COMP-3.
           10 WB-FPRDD-BETRAG-FREI        PIC S9(09)V9(02) COMP-3.
           10 WB-FPRDD-FAKTOR             PIC S9(03)V9(02) COMP-3.
           10 WB-FPRDD-ANZ-MONATE         PIC S9(04)       BINARY.
           10 WB-FPRDD-UMS-BED-ERF        PIC  X(01).
           10 WB-FPRDD-S-AUSZAHLUNG       PIC  X(01).
           10 WB-FPRDD-S-NACH-DIR         PIC  X(01).
200922     10 WB-FPRDD-12M-UMS-UPFL       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-03M-UMS-UPFL       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-PRO-ABZUG-UP       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-N-PRO-ABZ-UP       PIC S9(10)V9(01) COMP-3.
           10 WB-FPRDD-ZW-BETRAG-UP       PIC S9(09)V9(02) COMP-3.
           10 WB-FPRDD-EQF-ABZUG-UP       PIC S9(09)V9(02) COMP-3.
           10 WB-FPRDD-BETRAG-UPFL        PIC S9(09)V9(02) COMP-3.
200922     10 WB-FPRDD-ABZUG-NEG          PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-03M-KORR-FRE       PIC S9(10)V9(01) COMP-3.
200922     10 WB-FPRDD-03M-KORR-UPF       PIC S9(10)V9(01) COMP-3.
           10 WB-FPRDD-ANTEIL-FREI        PIC S9(03)V9(02) COMP-3.
           10 WB-FPRDD-ANTEIL-UPFL        PIC S9(03)V9(02) COMP-3.
 
      *****************************************************************
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  31                    *
      *****************************************************************

