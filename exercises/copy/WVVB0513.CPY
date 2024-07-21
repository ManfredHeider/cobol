      *****************************************************************         
      *    COPY-MEMBER: WVVB0513                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0513 (CABS    )                       *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: G.BRUDER                                         *         
      *    DATUM:    21.02.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4138 !21.02.22! NEU                           ! G.BRUDER  *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0513                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0513-TAB.                                                  
           10 WB-NI-VIVB0513-ELE   PIC S9(04)  BINARY OCCURS   6.               
        05 WB-NI-VIVB0513          REDEFINES   WB-NI-VIVB0513-TAB.              
           10 WB-NI-ST-NR                 PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ZUSTIMM-STAT      PIC S9(04)       BINARY.              
           10 WB-NI-CABS-DAT-BIS          PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ZUSTIMM-STAT      PIC S9(04)       BINARY.              
           10 WB-NI-CABS-DAT-VON          PIC S9(04)       BINARY.              
           10 WB-NI-CABS-LAST-UPDATE      PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVB0513.                                                         
           10 WB-ST-NR                    PIC S9(09)       BINARY.              
           10 WB-KNR-ZUSTIMM-STAT         PIC S9(04)       BINARY.              
           10 WB-CABS-DAT-BIS             PIC  X(10).                           
           10 WB-KTY-ZUSTIMM-STAT         PIC S9(04)       BINARY.              
           10 WB-CABS-DAT-VON             PIC  X(26).                           
           10 WB-CABS-LAST-UPDATE         PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *         
      *****************************************************************         
