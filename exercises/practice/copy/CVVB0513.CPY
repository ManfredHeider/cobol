      *****************************************************************         
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
      * DCLGEN TABLE(VIVB0513) CABS                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0513)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0513 TABLE                                      
CCC004*    ( ST_NR                       INTEGER NOT NULL                       
CCC004*    , KNR_ZUSTIMM_STAT            SMALLINT NOT NULL                      
CCC004*    , CABS_DAT_BIS                DATE NOT NULL                          
CCC004*    , KTY_ZUSTIMM_STAT            SMALLINT NOT NULL                      
CCC004*    , CABS_DAT_VON                TIMESTAMP NOT NULL                     
CCC004*    , CABS_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0513                          *         
      *****************************************************************         
                                                                                
       01  VIVB0513-GESAMT.                                                     
        05 NI-VIVB0513-TAB.                                                     
           10 NI-VIVB0513-ELE      PIC S9(04)  BINARY OCCURS   6.               
        05 NI-VIVB0513             REDEFINES   NI-VIVB0513-TAB.                 
           10 NI-ST-NR                    PIC S9(04)       BINARY.              
           10 NI-KNR-ZUSTIMM-STAT         PIC S9(04)       BINARY.              
           10 NI-CABS-DAT-BIS             PIC S9(04)       BINARY.              
           10 NI-KTY-ZUSTIMM-STAT         PIC S9(04)       BINARY.              
           10 NI-CABS-DAT-VON             PIC S9(04)       BINARY.              
           10 NI-CABS-LAST-UPDATE         PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVB0513.                                                        
           10 ST-NR                       PIC S9(09)       BINARY.              
           10 KNR-ZUSTIMM-STAT            PIC S9(04)       BINARY.              
           10 CABS-DAT-BIS                PIC  X(10).                           
           10 KTY-ZUSTIMM-STAT            PIC S9(04)       BINARY.              
           10 CABS-DAT-VON                PIC  X(26).                           
           10 CABS-LAST-UPDATE            PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :   6                    *         
      *****************************************************************         
