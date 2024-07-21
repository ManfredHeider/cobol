      *****************************************************************         
      *    COPY-MEMBER: WVVBC099                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVBC099 (NAMZ)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    14.03.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4411 !14.03.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBC099                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVBC099-TAB.                                                  
           10 WB-NI-VIVBC099-ELE   PIC S9(04)  BINARY OCCURS  14.               
        05 WB-NI-VIVBC099          REDEFINES   WB-NI-VIVBC099-TAB.              
           10 WB-NI-KTY-NAMZ-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-NAMZ-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-LFD              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ANREDE            PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ANREDE            PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-TITEL-AKAD       PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-NAME             PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-VORNAME          PIC S9(04)       BINARY.              
           10 WB-NI-KTY-NAMZ-FUNK         PIC S9(04)       BINARY.              
           10 WB-NI-KNR-NAMZ-FUNK         PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-DAT-VON          PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-DAT-BIS          PIC S9(04)       BINARY.              
           10 WB-NI-NAMZ-EMAIL            PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVBC099.                                                         
           10 WB-KTY-NAMZ-TYP             PIC S9(04)       BINARY.              
           10 WB-KNR-NAMZ-TYP             PIC S9(04)       BINARY.              
           10 WB-NAMZ-LFD                 PIC S9(04)       BINARY.              
           10 WB-KTY-ANREDE               PIC S9(04)       BINARY.              
           10 WB-KNR-ANREDE               PIC S9(04)       BINARY.              
           10 WB-NAMZ-TITEL-AKAD          PIC  X(10).                           
           10 WB-NAMZ-NAME                PIC  X(40).                           
           10 WB-NAMZ-VORNAME             PIC  X(20).                           
           10 WB-KTY-NAMZ-FUNK            PIC S9(04)       BINARY.              
           10 WB-KNR-NAMZ-FUNK            PIC S9(04)       BINARY.              
           10 WB-NAMZ-DAT-VON             PIC  X(26).                           
           10 WB-NAMZ-LAST-UPDATE         PIC  X(26).                           
           10 WB-NAMZ-DAT-BIS             PIC  X(10).                           
           10 WB-NAMZ-EMAIL               PIC  X(80).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *         
      *****************************************************************         
