      *****************************************************************         
      *    COPY-MEMBER: WVVB0146                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIVB0146 (KONZ)                           *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    11.05.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-4457 !11.05.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0146                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIVB0146-TAB.                                                  
           10 WB-NI-VIVB0146-ELE   PIC S9(04)  BINARY OCCURS  11.               
        05 WB-NI-VIVB0146          REDEFINES   WB-NI-VIVB0146-TAB.              
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-KONZ-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-KONZ-TYP          PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-ERF-DAT          PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-LFD-NR           PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-USER-ID          PIC S9(04)       BINARY.              
           10 WB-NI-KTY-STATUS-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-STATUS-TYP        PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-DATEN-ALT        PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-DATEN-NEU        PIC S9(04)       BINARY.              
           10 WB-NI-KONZ-LAST-UPD         PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIVB0146.                                                         
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.              
           10 WB-KTY-KONZ-TYP             PIC S9(04)       BINARY.              
           10 WB-KNR-KONZ-TYP             PIC S9(04)       BINARY.              
           10 WB-KONZ-ERF-DAT             PIC  X(10).                           
           10 WB-KONZ-LFD-NR              PIC S9(04)       BINARY.              
           10 WB-KONZ-USER-ID             PIC  X(08).                           
           10 WB-KTY-STATUS-TYP           PIC S9(04)       BINARY.              
           10 WB-KNR-STATUS-TYP           PIC S9(04)       BINARY.              
           10 WB-KONZ-DATEN-ALT           PIC  X(40).                           
           10 WB-KONZ-DATEN-NEU           PIC  X(40).                           
           10 WB-KONZ-LAST-UPD            PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *         
      *****************************************************************         
