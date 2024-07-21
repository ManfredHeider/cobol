      *****************************************************************         
      *    COPY-MEMBER: WVAT0061                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIAT0061 (BRIEFD)                         *         
      *    --------------------------------------------------------   *         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    31.05.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3405 !31.05.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIAT0061                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIAT0061-TAB.                                                  
           10 WB-NI-VIAT0061-ELE   PIC S9(04)  BINARY OCCURS  21.               
        05 WB-NI-VIAT0061          REDEFINES   WB-NI-VIAT0061-TAB.              
           10 WB-NI-BRIEF                 PIC S9(04)       BINARY.              
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.              
           10 WB-NI-KTY-DRUCK-STATUS      PIC S9(04)       BINARY.              
           10 WB-NI-KNR-DRUCK-STATUS      PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DAT-ERST       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DAT-DRUCK      PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DAT-BRIEF      PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-ONDEMAND       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-LADON          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DRUCK          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-EMAIL-VB       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-EMAIL-FREI     PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-OWNER          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DRU-IP         PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-DRU-NAME       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-FORM-DEF       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-FORMS          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-GESELL         PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-BEZ            PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-LAST-UPD       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFD-SIB            PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIAT0061.                                                         
           10 WB-BRIEF                    PIC  X(16).                           
           10 WB-MA-ST-NR                 PIC S9(09)       BINARY.              
           10 WB-KTY-DRUCK-STATUS         PIC S9(04)       BINARY.              
           10 WB-KNR-DRUCK-STATUS         PIC S9(04)       BINARY.              
           10 WB-BRIEFD-DAT-ERST          PIC  X(10).                           
           10 WB-BRIEFD-DAT-DRUCK         PIC  X(10).                           
           10 WB-BRIEFD-DAT-BRIEF         PIC  X(10).                           
           10 WB-BRIEFD-ONDEMAND          PIC  X(01).                           
           10 WB-BRIEFD-LADON             PIC  X(01).                           
           10 WB-BRIEFD-DRUCK             PIC  X(01).                           
           10 WB-BRIEFD-EMAIL-VB          PIC  X(80).                           
           10 WB-BRIEFD-EMAIL-FREI        PIC  X(80).                           
           10 WB-BRIEFD-OWNER             PIC  X(40).                           
           10 WB-BRIEFD-DRU-IP            PIC  X(32).                           
           10 WB-BRIEFD-DRU-NAME          PIC  X(16).                           
           10 WB-BRIEFD-FORM-DEF          PIC  X(16).                           
           10 WB-BRIEFD-FORMS             PIC  X(16).                           
           10 WB-BRIEFD-GESELL            PIC  X(15).                           
           10 WB-BRIEFD-BEZ               PIC  X(80).                           
           10 WB-BRIEFD-LAST-UPD          PIC  X(26).                           
           10 WB-BRIEFD-SIB.                                                    
              49 WB-BRIEFD-SIB-LAENGE     PIC S9(04)       BINARY.              
              49 WB-BRIEFD-SIB-INHALT     PIC  X(32000).                        
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  21                    *         
      *****************************************************************