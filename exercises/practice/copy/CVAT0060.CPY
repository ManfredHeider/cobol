      *****************************************************************         
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
      * DCLGEN TABLE(VIAT0060) BRIEFK                                 *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0060)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIAT0060 TABLE                                      
CCC004*    ( BRIEF                       CHAR(16) NOT NULL                      
CCC004*    , BRIEFK_ONDEMAND             CHAR(01) NOT NULL                      
CCC004*    , BRIEFK_LADON                CHAR(01) NOT NULL                      
CCC004*    , BRIEFK_DRUCK                CHAR(01) NOT NULL                      
CCC004*    , BRIEFK_EMAIL_VB             CHAR(01) NOT NULL                      
CCC004*    , BRIEFK_EMAIL_FREI           CHAR(80)                               
CCC004*    , BRIEFK_OWNER                CHAR(40)                               
CCC004*    , BRIEFK_DRU_IP               CHAR(32)                               
CCC004*    , BRIEFK_DRU_NAME             CHAR(32)                               
CCC004*    , BRIEFK_FORM_DEF             CHAR(16)                               
CCC004*    , BRIEFK_FORMS                CHAR(16)                               
CCC004*    , BRIEFK_GESELL               CHAR(15)                               
CCC004*    , BRIEFK_BEZ                  CHAR(80)                               
CCC004*    , BRIEFK_LAST_UPD             TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIAT0060                          *         
      *****************************************************************         
                                                                                
       01  VIAT0060-GESAMT.                                                     
        05 NI-VIAT0060-TAB.                                                     
           10 NI-VIAT0060-ELE      PIC S9(04)  BINARY OCCURS  14.               
        05 NI-VIAT0060             REDEFINES   NI-VIAT0060-TAB.                 
           10 NI-BRIEF                    PIC S9(04)       BINARY.              
           10 NI-BRIEFK-ONDEMAND          PIC S9(04)       BINARY.              
           10 NI-BRIEFK-LADON             PIC S9(04)       BINARY.              
           10 NI-BRIEFK-DRUCK             PIC S9(04)       BINARY.              
           10 NI-BRIEFK-EMAIL-VB          PIC S9(04)       BINARY.              
           10 NI-BRIEFK-EMAIL-FREI        PIC S9(04)       BINARY.              
           10 NI-BRIEFK-OWNER             PIC S9(04)       BINARY.              
           10 NI-BRIEFK-DRU-IP            PIC S9(04)       BINARY.              
           10 NI-BRIEFK-DRU-NAME          PIC S9(04)       BINARY.              
           10 NI-BRIEFK-FORM-DEF          PIC S9(04)       BINARY.              
           10 NI-BRIEFK-FORMS             PIC S9(04)       BINARY.              
           10 NI-BRIEFK-GESELL            PIC S9(04)       BINARY.              
           10 NI-BRIEFK-BEZ               PIC S9(04)       BINARY.              
           10 NI-BRIEFK-LAST-UPD          PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIAT0060.                                                        
           10 BRIEF                       PIC  X(16).                           
           10 BRIEFK-ONDEMAND             PIC  X(01).                           
           10 BRIEFK-LADON                PIC  X(01).                           
           10 BRIEFK-DRUCK                PIC  X(01).                           
           10 BRIEFK-EMAIL-VB             PIC  X(01).                           
           10 BRIEFK-EMAIL-FREI           PIC  X(80).                           
           10 BRIEFK-OWNER                PIC  X(40).                           
           10 BRIEFK-DRU-IP               PIC  X(32).                           
           10 BRIEFK-DRU-NAME             PIC  X(32).                           
           10 BRIEFK-FORM-DEF             PIC  X(16).                           
           10 BRIEFK-FORMS                PIC  X(16).                           
           10 BRIEFK-GESELL               PIC  X(15).                           
           10 BRIEFK-BEZ                  PIC  X(80).                           
           10 BRIEFK-LAST-UPD             PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  14                    *         
      *****************************************************************