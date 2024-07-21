      *****************************************************************         
      *    COPY-MEMBER: WVVBD015                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVBD015 (MAAN    )                       *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    20.05.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-2627 !20.05.21! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBD015                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVBD015-TAB.                                                  
           10 WB-NI-VIVBD015-ELE   PIC S9(04)  BINARY OCCURS  30.               
        05 WB-NI-VIVBD015          REDEFINES   WB-NI-VIVBD015-TAB.              
           10 WB-NI-MAAN-BEZUGS-NR        PIC S9(04)       BINARY.              
           10 WB-NI-KTY-MAAN-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-MAAN-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-DAT-VON          PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-STRASSE          PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-PLZ-AKTUELL      PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-PLZ              PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-ORT              PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-PLZ-PF           PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-ORT-PF           PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-POSTFACH         PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-WOHNT-BEI        PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-DAT-BIS          PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-TELEFON          PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-TELEFAX          PIC S9(04)       BINARY.              
           10 WB-NI-KTY-AUSL              PIC S9(04)       BINARY.              
           10 WB-NI-KNR-AUSL              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-GRND              PIC S9(04)       BINARY.              
           10 WB-NI-KNR-GRND              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ZUSTELLUNG        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ZUSTELLUNG        PIC S9(04)       BINARY.              
           10 WB-NI-KTY-BEZUGS-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-BEZUGS-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KTY-GEWERBE           PIC S9(04)       BINARY.              
           10 WB-NI-KNR-GEWERBE           PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-STRASSE-GK       PIC S9(04)       BINARY.              
           10 WB-NI-MAAN-ORT-GK           PIC S9(04)       BINARY.              
           10 WB-NI-KTY-BETRST            PIC S9(04)       BINARY.              
           10 WB-NI-KNR-BETRST            PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVBD015.                                                         
           10 WB-MAAN-BEZUGS-NR           PIC S9(09)       BINARY.              
           10 WB-KTY-MAAN-TYP             PIC S9(04)       BINARY.              
           10 WB-KNR-MAAN-TYP             PIC S9(04)       BINARY.              
           10 WB-MAAN-DAT-VON             PIC  X(26).                           
           10 WB-MAAN-LAST-UPDATE         PIC  X(26).                           
           10 WB-MAAN-STRASSE             PIC  X(40).                           
           10 WB-MAAN-PLZ-AKTUELL         PIC  X(09).                           
           10 WB-MAAN-PLZ                 PIC  X(09).                           
           10 WB-MAAN-ORT                 PIC  X(40).                           
           10 WB-MAAN-PLZ-PF              PIC  X(09).                           
           10 WB-MAAN-ORT-PF              PIC  X(40).                           
           10 WB-MAAN-POSTFACH            PIC  X(06).                           
           10 WB-MAAN-WOHNT-BEI           PIC  X(40).                           
           10 WB-MAAN-DAT-BIS             PIC  X(10).                           
           10 WB-MAAN-TELEFON             PIC  X(26).                           
           10 WB-MAAN-TELEFAX             PIC  X(26).                           
           10 WB-KTY-AUSL                 PIC S9(04)       BINARY.              
           10 WB-KNR-AUSL                 PIC S9(04)       BINARY.              
           10 WB-KTY-GRND                 PIC S9(04)       BINARY.              
           10 WB-KNR-GRND                 PIC S9(04)       BINARY.              
           10 WB-KTY-ZUSTELLUNG           PIC S9(04)       BINARY.              
           10 WB-KNR-ZUSTELLUNG           PIC S9(04)       BINARY.              
           10 WB-KTY-BEZUGS-TYP           PIC S9(04)       BINARY.              
           10 WB-KNR-BEZUGS-TYP           PIC S9(04)       BINARY.              
           10 WB-KTY-GEWERBE              PIC S9(04)       BINARY.              
           10 WB-KNR-GEWERBE              PIC S9(04)       BINARY.              
           10 WB-MAAN-STRASSE-GK          PIC  X(40).                           
           10 WB-MAAN-ORT-GK              PIC  X(40).                           
           10 WB-KTY-BETRST               PIC S9(04)       BINARY.              
           10 WB-KNR-BETRST               PIC S9(04)       BINARY.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  30                    *         
      *****************************************************************         
