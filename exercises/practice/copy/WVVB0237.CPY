      *****************************************************************         
      *    COPY-MEMBER: WVVB0237                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0237 (BZLP)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    01.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !01.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0237                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0237-TAB.                                                  
           10 WB-NI-VIVB0237-ELE   PIC S9(004) BINARY OCCURS  15.               
        05 WB-NI-VIVB0237          REDEFINES   WB-NI-VIVB0237-TAB.              
           10 WB-NI-BZLP-LFDNR           PIC S9(004) BINARY.                    
           10 WB-NI-KTY-GESELL-TYP       PIC S9(004) BINARY.                    
           10 WB-NI-KNR-GESELL-TYP       PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-BIS-STAT        PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-ART             PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-EQF-1           PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-EQF-2           PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-EH-PROZ         PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-VB-VERTR        PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-BS              PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-BS-PROZ         PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-MS-NR           PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-VBP-NR          PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-STATUS          PIC S9(004) BINARY.                    
           10 WB-NI-BZLP-LAST-UPDATE     PIC S9(004) BINARY.                    
                                                                                
        05 WB-VIVB0237.                                                         
           10 WB-BZLP-LFDNR              PIC S9(004) BINARY.                    
           10 WB-KTY-GESELL-TYP          PIC S9(004) BINARY.                    
           10 WB-KNR-GESELL-TYP          PIC S9(004) BINARY.                    
           10 WB-BZLP-BIS-STAT           PIC  X(010).                           
           10 WB-BZLP-ART                PIC  X(001).                           
           10 WB-BZLP-EQF-1              PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLP-EQF-2              PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLP-EH-PROZ            PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLP-VB-VERTR           PIC S9(004) BINARY.                    
           10 WB-BZLP-BS                 PIC  X(001).                           
           10 WB-BZLP-BS-PROZ            PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLP-MS-NR              PIC S9(004) BINARY.                    
           10 WB-BZLP-VBP-NR             PIC S9(004) BINARY.                    
           10 WB-BZLP-STATUS             PIC  X(001).                           
           10 WB-BZLP-LAST-UPDATE        PIC  X(026).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *         
      *****************************************************************         
