      *****************************************************************         
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
      * DCLGEN TABLE(VIVBC099) NAMZ                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBC099)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVBC099 TABLE                                      
CCC004*    ( KTY_NAMZ_TYP                SMALLINT NOT NULL                      
CCC004*    , KNR_NAMZ_TYP                SMALLINT NOT NULL                      
CCC004*    , NAMZ_LFD                    SMALLINT NOT NULL                      
CCC004*    , KTY_ANREDE                  SMALLINT NOT NULL                      
CCC004*    , KNR_ANREDE                  SMALLINT NOT NULL                      
CCC004*    , NAMZ_TITEL_AKAD             CHAR(10)                               
CCC004*    , NAMZ_NAME                   CHAR(40) NOT NULL                      
CCC004*    , NAMZ_VORNAME                CHAR(20) NOT NULL                      
CCC004*    , KTY_NAMZ_FUNK               SMALLINT NOT NULL                      
CCC004*    , KNR_NAMZ_FUNK               SMALLINT NOT NULL                      
CCC004*    , NAMZ_DAT_VON                TIMESTAMP NOT NULL                     
CCC004*    , NAMZ_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC004*    , NAMZ_DAT_BIS                DATE NOT NULL                          
CCC004*    , NAMZ_EMAIL                  CHAR(80)                               
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBC099                          *         
      *****************************************************************         
                                                                                
       01  VIVBC099-GESAMT.                                                     
        05 NI-VIVBC099-TAB.                                                     
           10 NI-VIVBC099-ELE      PIC S9(04)  BINARY OCCURS  14.               
        05 NI-VIVBC099             REDEFINES   NI-VIVBC099-TAB.                 
           10 NI-KTY-NAMZ-TYP             PIC S9(04)       BINARY.              
           10 NI-KNR-NAMZ-TYP             PIC S9(04)       BINARY.              
           10 NI-NAMZ-LFD                 PIC S9(04)       BINARY.              
           10 NI-KTY-ANREDE               PIC S9(04)       BINARY.              
           10 NI-KNR-ANREDE               PIC S9(04)       BINARY.              
           10 NI-NAMZ-TITEL-AKAD          PIC S9(04)       BINARY.              
           10 NI-NAMZ-NAME                PIC S9(04)       BINARY.              
           10 NI-NAMZ-VORNAME             PIC S9(04)       BINARY.              
           10 NI-KTY-NAMZ-FUNK            PIC S9(04)       BINARY.              
           10 NI-KNR-NAMZ-FUNK            PIC S9(04)       BINARY.              
           10 NI-NAMZ-DAT-VON             PIC S9(04)       BINARY.              
           10 NI-NAMZ-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-NAMZ-DAT-BIS             PIC S9(04)       BINARY.              
           10 NI-NAMZ-EMAIL               PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVBC099.                                                        
           10 KTY-NAMZ-TYP                PIC S9(04)       BINARY.              
           10 KNR-NAMZ-TYP                PIC S9(04)       BINARY.              
           10 NAMZ-LFD                    PIC S9(04)       BINARY.              
           10 KTY-ANREDE                  PIC S9(04)       BINARY.              
           10 KNR-ANREDE                  PIC S9(04)       BINARY.              
           10 NAMZ-TITEL-AKAD             PIC  X(10).                           
           10 NAMZ-NAME                   PIC  X(40).                           
           10 NAMZ-VORNAME                PIC  X(20).                           
           10 KTY-NAMZ-FUNK               PIC S9(04)       BINARY.              
           10 KNR-NAMZ-FUNK               PIC S9(04)       BINARY.              
           10 NAMZ-DAT-VON                PIC  X(26).                           
           10 NAMZ-LAST-UPDATE            PIC  X(26).                           
           10 NAMZ-DAT-BIS                PIC  X(10).                           
           10 NAMZ-EMAIL                  PIC  X(80).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *         
      *****************************************************************         
