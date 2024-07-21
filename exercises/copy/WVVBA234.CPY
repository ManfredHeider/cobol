      *****************************************************************         
      *    COPY-MEMBER: WVVBA234                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVBA234 (FAPP)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    10.11.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3687 !10.11.21! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBA234                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVBA234-TAB.                                                  
           10 WB-NI-VIVBA234-ELE   PIC S9(04)  BINARY OCCURS  13.               
        05 WB-NI-VIVBA234          REDEFINES   WB-NI-VIVBA234-TAB.              
           10 WB-NI-FAPP-LFDNR            PIC S9(04)       BINARY.              
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-BIS-STAT         PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-ART              PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-EQF              PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-EH-PROZ          PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-VB-VERTR         PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-BS               PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-MIN-PROZ         PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-STATUS           PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-FAPP-EQF-2            PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVBA234.                                                         
           10 WB-FAPP-LFDNR               PIC S9(04)       BINARY.              
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.              
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.              
           10 WB-FAPP-BIS-STAT            PIC  X(10).                           
           10 WB-FAPP-ART                 PIC  X(01).                           
           10 WB-FAPP-EQF                 PIC S9(03)V9(02) COMP-3.              
           10 WB-FAPP-EH-PROZ             PIC S9(03)V9(02) COMP-3.              
           10 WB-FAPP-VB-VERTR            PIC S9(04)       BINARY.              
           10 WB-FAPP-BS                  PIC  X(01).                           
           10 WB-FAPP-MIN-PROZ            PIC S9(03)V9(02) COMP-3.              
           10 WB-FAPP-STATUS              PIC  X(01).                           
           10 WB-FAPP-LAST-UPDATE         PIC  X(26).                           
           10 WB-FAPP-EQF-2               PIC S9(03)V9(02) COMP-3.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *         
      *****************************************************************         
