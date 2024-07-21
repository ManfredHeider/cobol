      *****************************************************************         
      *    COPY-MEMBER: WVAT0060                                      *         
      *    --------------------------------------------------------   *         
      *    WORKBEREICH FUER VIAT0060 (BRIEFK)                         *         
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
      * COBOL DEKLARATION FUER VIEW VIAT0060                          *         
      *****************************************************************         
                                                                                
        05 WB-NI-VIAT0060-TAB.                                                  
           10 WB-NI-VIAT0060-ELE   PIC S9(04)  BINARY OCCURS  14.               
        05 WB-NI-VIAT0060          REDEFINES   WB-NI-VIAT0060-TAB.              
           10 WB-NI-BRIEF                 PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-ONDEMAND       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-LADON          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-DRUCK          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-EMAIL-VB       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-EMAIL-FREI     PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-OWNER          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-DRU-IP         PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-DRU-NAME       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-FORM-DEF       PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-FORMS          PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-GESELL         PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-BEZ            PIC S9(04)       BINARY.              
           10 WB-NI-BRIEFK-LAST-UPD       PIC S9(04)       BINARY.              
                                                                                
        05 WB-VIAT0060.                                                         
           10 WB-BRIEF                    PIC  X(16).                           
           10 WB-BRIEFK-ONDEMAND          PIC  X(01).                           
           10 WB-BRIEFK-LADON             PIC  X(01).                           
           10 WB-BRIEFK-DRUCK             PIC  X(01).                           
           10 WB-BRIEFK-EMAIL-VB          PIC  X(01).                           
           10 WB-BRIEFK-EMAIL-FREI        PIC  X(80).                           
           10 WB-BRIEFK-OWNER             PIC  X(40).                           
           10 WB-BRIEFK-DRU-IP            PIC  X(32).                           
           10 WB-BRIEFK-DRU-NAME          PIC  X(32).                           
           10 WB-BRIEFK-FORM-DEF          PIC  X(16).                           
           10 WB-BRIEFK-FORMS             PIC  X(16).                           
           10 WB-BRIEFK-GESELL            PIC  X(15).                           
           10 WB-BRIEFK-BEZ               PIC  X(80).                           
           10 WB-BRIEFK-LAST-UPD          PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *         
      *****************************************************************