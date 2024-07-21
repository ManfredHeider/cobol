      *****************************************************************         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    10.11.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3687 !10.11.21! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * DCLGEN TABLE(VIVBA234) FAPP                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA234)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVBA234 TABLE                                      
CCC004*    ( FAPP_LFDNR                  SMALLINT NOT NULL                      
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , FAPP_BIS_STAT               DATE NOT NULL                          
CCC004*    , FAPP_ART                    CHAR(01) NOT NULL                      
CCC004*    , FAPP_EQF                    DECIMAL(05, 2)                         
CCC004*    , FAPP_EH_PROZ                DECIMAL(05, 2)                         
CCC004*    , FAPP_VB_VERTR               SMALLINT                               
CCC004*    , FAPP_BS                     CHAR(01) NOT NULL                      
CCC004*    , FAPP_MIN_PROZ               DECIMAL(05, 2)                         
CCC004*    , FAPP_STATUS                 CHAR(01) NOT NULL                      
CCC004*    , FAPP_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC004*    , FAPP_EQF_2                  DECIMAL(05, 2) NOT NULL                
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBA234                          *         
      *****************************************************************         
                                                                                
       01  VIVBA234-GESAMT.                                                     
        05 NI-VIVBA234-TAB.                                                     
           10 NI-VIVBA234-ELE      PIC S9(04)  BINARY OCCURS  13.               
        05 NI-VIVBA234             REDEFINES   NI-VIVBA234-TAB.                 
           10 NI-FAPP-LFDNR               PIC S9(04)       BINARY.              
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.              
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.              
           10 NI-FAPP-BIS-STAT            PIC S9(04)       BINARY.              
           10 NI-FAPP-ART                 PIC S9(04)       BINARY.              
           10 NI-FAPP-EQF                 PIC S9(04)       BINARY.              
           10 NI-FAPP-EH-PROZ             PIC S9(04)       BINARY.              
           10 NI-FAPP-VB-VERTR            PIC S9(04)       BINARY.              
           10 NI-FAPP-BS                  PIC S9(04)       BINARY.              
           10 NI-FAPP-MIN-PROZ            PIC S9(04)       BINARY.              
           10 NI-FAPP-STATUS              PIC S9(04)       BINARY.              
           10 NI-FAPP-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-FAPP-EQF-2               PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVBA234.                                                        
           10 FAPP-LFDNR                  PIC S9(04)       BINARY.              
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.              
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.              
           10 FAPP-BIS-STAT               PIC  X(10).                           
           10 FAPP-ART                    PIC  X(01).                           
           10 FAPP-EQF                    PIC S9(03)V9(02) COMP-3.              
           10 FAPP-EH-PROZ                PIC S9(03)V9(02) COMP-3.              
           10 FAPP-VB-VERTR               PIC S9(04)       BINARY.              
           10 FAPP-BS                     PIC  X(01).                           
           10 FAPP-MIN-PROZ               PIC S9(03)V9(02) COMP-3.              
           10 FAPP-STATUS                 PIC  X(01).                           
           10 FAPP-LAST-UPDATE            PIC  X(26).                           
           10 FAPP-EQF-2                  PIC S9(03)V9(02) COMP-3.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  13                    *         
      *****************************************************************         
