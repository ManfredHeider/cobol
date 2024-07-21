      *****************************************************************         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    01.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !01.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * DCLGEN TABLE(VIVB0237) BZLP                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0237)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0237 TABLE                                      
CCC004*    ( BZLP_LFDNR                  SMALLINT NOT NULL                      
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , BZLP_BIS_STAT               DATE NOT NULL                          
CCC004*    , BZLP_ART                    CHAR(001) NOT NULL                     
CCC004*    , BZLP_EQF_1                  DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLP_EQF_2                  DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLP_EH_PROZ                DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLP_VB_VERTR               SMALLINT NOT NULL                      
CCC004*    , BZLP_BS                     CHAR(001) NOT NULL                     
CCC004*    , BZLP_BS_PROZ                DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLP_MS_NR                  SMALLINT NOT NULL                      
CCC004*    , BZLP_VBP_NR                 SMALLINT NOT NULL                      
CCC004*    , BZLP_STATUS                 CHAR(001) NOT NULL                     
CCC004*    , BZLP_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0237                          *         
      *****************************************************************         
                                                                                
       01 VIVB0237-GESAMT.                                                      
          05 NI-VIVB0237-TAB.                                                   
             10 NI-VIVB0237-ELE    PIC S9(004) BINARY OCCURS  15.               
          05 NI-VIVB0237           REDEFINES   NI-VIVB0237-TAB.                 
             10 NI-BZLP-LFDNR            PIC S9(004) BINARY.                    
             10 NI-KTY-GESELL-TYP        PIC S9(004) BINARY.                    
             10 NI-KNR-GESELL-TYP        PIC S9(004) BINARY.                    
             10 NI-BZLP-BIS-STAT         PIC S9(004) BINARY.                    
             10 NI-BZLP-ART              PIC S9(004) BINARY.                    
             10 NI-BZLP-EQF-1            PIC S9(004) BINARY.                    
             10 NI-BZLP-EQF-2            PIC S9(004) BINARY.                    
             10 NI-BZLP-EH-PROZ          PIC S9(004) BINARY.                    
             10 NI-BZLP-VB-VERTR         PIC S9(004) BINARY.                    
             10 NI-BZLP-BS               PIC S9(004) BINARY.                    
             10 NI-BZLP-BS-PROZ          PIC S9(004) BINARY.                    
             10 NI-BZLP-MS-NR            PIC S9(004) BINARY.                    
             10 NI-BZLP-VBP-NR           PIC S9(004) BINARY.                    
             10 NI-BZLP-STATUS           PIC S9(004) BINARY.                    
             10 NI-BZLP-LAST-UPDATE      PIC S9(004) BINARY.                    
                                                                                
          05 DCL-VIVB0237.                                                      
             10 BZLP-LFDNR               PIC S9(004) BINARY.                    
             10 KTY-GESELL-TYP           PIC S9(004) BINARY.                    
             10 KNR-GESELL-TYP           PIC S9(004) BINARY.                    
             10 BZLP-BIS-STAT            PIC  X(010).                           
             10 BZLP-ART                 PIC  X(001).                           
             10 BZLP-EQF-1               PIC S9(003)V9(002) COMP-3.             
             10 BZLP-EQF-2               PIC S9(003)V9(002) COMP-3.             
             10 BZLP-EH-PROZ             PIC S9(003)V9(002) COMP-3.             
             10 BZLP-VB-VERTR            PIC S9(004) BINARY.                    
             10 BZLP-BS                  PIC  X(001).                           
             10 BZLP-BS-PROZ             PIC S9(003)V9(002) COMP-3.             
             10 BZLP-MS-NR               PIC S9(004) BINARY.                    
             10 BZLP-VBP-NR              PIC S9(004) BINARY.                    
             10 BZLP-STATUS              PIC  X(001).                           
             10 BZLP-LAST-UPDATE         PIC  X(026).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  15                    *         
      *****************************************************************         
