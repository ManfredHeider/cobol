      *****************************************************************         
      *    COPY-MEMBER: WVVB0238                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0238 (BZLE)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    05.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !05.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0238                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0238-TAB.                                                  
           10 WB-NI-VIVB0238-ELE   PIC S9(004) BINARY OCCURS  34.               
        05 WB-NI-VIVB0238          REDEFINES   WB-NI-VIVB0238-TAB.              
           10 WB-NI-BZLE-LFDNR           PIC S9(004) BINARY.                    
           10 WB-NI-KTY-GESELL-TYP       PIC S9(004) BINARY.                    
           10 WB-NI-KNR-GESELL-TYP       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ART             PIC S9(004) BINARY.                    
           10 WB-NI-MA-ST-NR             PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ERM-KZ          PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-MS-NR           PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-VBP-NR          PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-VBP-NR-G        PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ZV-NR           PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ZV-DAT-BIS      PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ZV-DAT-VBG      PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-EG-NEU       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-EG-NEU-A     PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ERF-KZ-EG       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-JEQF-EG         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-EG-GES       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-EG-KORR      PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-GR-GES       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-GES          PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-EH-GES-A        PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ERF-KZ-GS       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-JEQF-GES        PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-PROV-VERD       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-VBG-ABSICH      PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-CEN-VNR         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-CKV-NEU         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ASCLUB-DAT      PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-ASCLUB-BL       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-BS-VORJ         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-INFO-KZ         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-VERTRAG         PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-DAT-VOLLM       PIC S9(004) BINARY.                    
           10 WB-NI-BZLE-LAST-UPDATE     PIC S9(004) BINARY.                    
                                                                                
        05 WB-VIVB0238.                                                         
           10 WB-BZLE-LFDNR              PIC S9(004) BINARY.                    
           10 WB-KTY-GESELL-TYP          PIC S9(004) BINARY.                    
           10 WB-KNR-GESELL-TYP          PIC S9(004) BINARY.                    
           10 WB-BZLE-ART                PIC  X(001).                           
           10 WB-MA-ST-NR                PIC S9(009) BINARY.                    
           10 WB-BZLE-ERM-KZ             PIC  X(002).                           
           10 WB-BZLE-MS-NR              PIC S9(004) BINARY.                    
           10 WB-BZLE-VBP-NR             PIC S9(004) BINARY.                    
           10 WB-BZLE-VBP-NR-G           PIC S9(004) BINARY.                    
           10 WB-BZLE-ZV-NR              PIC  X(001).                           
           10 WB-BZLE-ZV-DAT-BIS         PIC  X(010).                           
           10 WB-BZLE-ZV-DAT-VBG         PIC  X(010).                           
           10 WB-BZLE-EH-EG-NEU          PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-EH-EG-NEU-A        PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-ERF-KZ-EG          PIC  X(001).                           
           10 WB-BZLE-JEQF-EG            PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLE-EH-EG-GES          PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-EH-EG-KORR         PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-EH-GR-GES          PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-EH-GES             PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-EH-GES-A           PIC S9(009)V9(001) COMP-3.             
           10 WB-BZLE-ERF-KZ-GS          PIC  X(001).                           
           10 WB-BZLE-JEQF-GES           PIC S9(003)V9(002) COMP-3.             
           10 WB-BZLE-PROV-VERD          PIC S9(009)V9(002) COMP-3.             
           10 WB-BZLE-VBG-ABSICH         PIC S9(009)V9(002) COMP-3.             
           10 WB-BZLE-CEN-VNR            PIC  X(015).                           
           10 WB-BZLE-CKV-NEU            PIC  X(001).                           
           10 WB-BZLE-ASCLUB-DAT         PIC  X(010).                           
           10 WB-BZLE-ASCLUB-BL          PIC  X(001).                           
           10 WB-BZLE-BS-VORJ            PIC  X(001).                           
           10 WB-BZLE-INFO-KZ            PIC  X(002).                           
           10 WB-BZLE-VERTRAG            PIC  X(001).                           
           10 WB-BZLE-DAT-VOLLM          PIC  X(010).                           
           10 WB-BZLE-LAST-UPDATE        PIC  X(026).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  34                    *         
      *****************************************************************         
