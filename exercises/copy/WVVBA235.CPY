      *****************************************************************         
      *    COPY-MEMBER: WVVBA235                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVBA235 (FAPE)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    06.01.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3688 !06.01.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBA235                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVBA235-TAB.                                                  
           10 WB-NI-VIVBA235-ELE   PIC S9(04)  BINARY OCCURS  27.               
        05 WB-NI-VIVBA235          REDEFINES   WB-NI-VIVBA235-TAB.              
           10 WB-NI-FAPE-LFDNR            PIC S9(04)       BINARY.              
           10 WB-NI-KTY-GESELL-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-GESELL-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-ART              PIC S9(04)       BINARY.              
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-ERM-KZ           PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-MS-NR            PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-VBP-NR           PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-EG-NEU        PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-EG-GES        PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-GES           PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-GES-KORR      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-GES-3MON      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-JEQF-EG          PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-JEQF-GES         PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-BES              PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-ALTERSGR-KZ      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EINTR-KUEND      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-MAT-DAT-BIS      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-ERF-KZ-EG        PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-ERF-KZ-GS        PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-STUFE            PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-STUFE-DVAG       PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-EG-NEU-A      PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-GES-A         PIC S9(04)       BINARY.              
           10 WB-NI-FAPE-EH-GES-3M-A      PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVBA235.                                                         
           10 WB-FAPE-LFDNR               PIC S9(04)       BINARY.              
           10 WB-KTY-GESELL-TYP           PIC S9(04)       BINARY.              
           10 WB-KNR-GESELL-TYP           PIC S9(04)       BINARY.              
           10 WB-FAPE-ART                 PIC  X(01).                           
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.              
           10 WB-FAPE-ERM-KZ              PIC  X(02).                           
           10 WB-FAPE-LAST-UPDATE         PIC  X(26).                           
           10 WB-FAPE-MS-NR               PIC S9(04)       BINARY.              
           10 WB-FAPE-VBP-NR              PIC S9(04)       BINARY.              
           10 WB-FAPE-EH-EG-NEU           PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-EG-GES           PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-GES              PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-GES-KORR         PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-GES-3MON         PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-JEQF-EG             PIC S9(03)V9(02) COMP-3.              
           10 WB-FAPE-JEQF-GES            PIC S9(03)V9(02) COMP-3.              
           10 WB-FAPE-BES                 PIC S9(04)       BINARY.              
           10 WB-FAPE-ALTERSGR-KZ         PIC  X(01).                           
           10 WB-FAPE-EINTR-KUEND         PIC  X(10).                           
           10 WB-FAPE-MAT-DAT-BIS         PIC  X(10).                           
           10 WB-FAPE-ERF-KZ-EG           PIC  X(01).                           
           10 WB-FAPE-ERF-KZ-GS           PIC  X(01).                           
           10 WB-FAPE-STUFE               PIC  X(01).                           
           10 WB-FAPE-STUFE-DVAG          PIC  X(01).                           
           10 WB-FAPE-EH-EG-NEU-A         PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-GES-A            PIC S9(09)V9(01) COMP-3.              
           10 WB-FAPE-EH-GES-3M-A         PIC S9(09)V9(01) COMP-3.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  27                    *         
      *****************************************************************         
