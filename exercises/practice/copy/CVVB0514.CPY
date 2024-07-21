      *****************************************************************         
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
      * DCLGEN TABLE(VIVB0514) CABB                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0514)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0514 TABLE                                      
CCC004*    ( ST_NR                       INTEGER NOT NULL                       
CCC004*    , CABB_ERM_VOM                DATE NOT NULL                          
CCC004*    , KNR_ERM_ART                 SMALLINT NOT NULL                      
CCC004*    , KTY_ERM_ART                 SMALLINT NOT NULL                      
CCC004*    , CABB_MS_NR                  SMALLINT NOT NULL                      
CCC004*    , CABB_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC004*    , KTY_WAEHRUNG                SMALLINT NOT NULL                      
CCC004*    , KNR_WAEHRUNG                SMALLINT NOT NULL                      
CCC004*    , CABB_NETTO_BETR             DECIMAL(11, 2) NOT NULL                
CCC004*    , CABB_BRUTTO_BETR            DECIMAL(11, 2) NOT NULL                
CCC004*    , KTY_BERECH_STAT             SMALLINT NOT NULL                      
CCC004*    , KNR_BERECH_STAT             SMALLINT NOT NULL                      
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0514                          *         
      *****************************************************************         
                                                                                
       01  VIVB0514-GESAMT.                                                     
        05 NI-VIVB0514-TAB.                                                     
           10 NI-VIVB0514-ELE      PIC S9(04)  BINARY OCCURS  12.               
        05 NI-VIVB0514             REDEFINES   NI-VIVB0514-TAB.                 
           10 NI-ST-NR                    PIC S9(04)       BINARY.              
           10 NI-CABB-ERM-VOM             PIC S9(04)       BINARY.              
           10 NI-KNR-ERM-ART              PIC S9(04)       BINARY.              
           10 NI-KTY-ERM-ART              PIC S9(04)       BINARY.              
           10 NI-CABB-MS-NR               PIC S9(04)       BINARY.              
           10 NI-CABB-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.              
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.              
           10 NI-CABB-NETTO-BETR          PIC S9(04)       BINARY.              
           10 NI-CABB-BRUTTO-BETR         PIC S9(04)       BINARY.              
           10 NI-KTY-BERECH-STAT          PIC S9(04)       BINARY.              
           10 NI-KNR-BERECH-STAT          PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVB0514.                                                        
           10 ST-NR                       PIC S9(09)       BINARY.              
           10 CABB-ERM-VOM                PIC  X(10).                           
           10 KNR-ERM-ART                 PIC S9(04)       BINARY.              
           10 KTY-ERM-ART                 PIC S9(04)       BINARY.              
           10 CABB-MS-NR                  PIC S9(04)       BINARY.              
           10 CABB-LAST-UPDATE            PIC  X(26).                           
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.              
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.              
           10 CABB-NETTO-BETR             PIC S9(09)V9(02) COMP-3.              
           10 CABB-BRUTTO-BETR            PIC S9(09)V9(02) COMP-3.              
           10 KTY-BERECH-STAT             PIC S9(04)       BINARY.              
           10 KNR-BERECH-STAT             PIC S9(04)       BINARY.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  12                    *         
      *****************************************************************         
