      *****************************************************************         
      *    COPY-MEMBER: WVVB0514                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0514 (CABB    )                       *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: G.BRUDER                                         *         
      *    DATUM:    22.02.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4138 !22.02.22! NEU                           ! G.BRUDER  *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0514                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0514-TAB.                                                  
           10 WB-NI-VIVB0514-ELE   PIC S9(04)  BINARY OCCURS  12.               
        05 WB-NI-VIVB0514          REDEFINES   WB-NI-VIVB0514-TAB.              
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.              
           10 WB-NI-CABB-ERM-VOM          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ERM-ART           PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ERM-ART           PIC S9(04)       BINARY.              
           10 WB-NI-CABB-MS-NR            PIC S9(04)       BINARY.              
           10 WB-NI-CABB-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.              
           10 WB-NI-CABB-NETTO-BETR       PIC S9(04)       BINARY.              
           10 WB-NI-CABB-BRUTTO-BETR      PIC S9(04)       BINARY.              
           10 WB-NI-KTY-BERECH-STAT       PIC S9(04)       BINARY.              
           10 WB-NI-KNR-BERECH-STAT       PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVB0514.                                                         
           10 WB-ST-NR                    PIC S9(09)       BINARY.              
           10 WB-CABB-ERM-VOM             PIC  X(10).                           
           10 WB-KNR-ERM-ART              PIC S9(04)       BINARY.              
           10 WB-KTY-ERM-ART              PIC S9(04)       BINARY.              
           10 WB-CABB-MS-NR               PIC S9(04)       BINARY.              
           10 WB-CABB-LAST-UPDATE         PIC  X(26).                           
           10 WB-KTY-WAEHRUNG             PIC S9(04)       BINARY.              
           10 WB-KNR-WAEHRUNG             PIC S9(04)       BINARY.              
           10 WB-CABB-NETTO-BETR          PIC S9(09)V9(02) COMP-3.              
           10 WB-CABB-BRUTTO-BETR         PIC S9(09)V9(02) COMP-3.              
           10 WB-KTY-BERECH-STAT          PIC S9(04)       BINARY.              
           10 WB-KNR-BERECH-STAT          PIC S9(04)       BINARY.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *         
      *****************************************************************         
