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
      * DCLGEN TABLE(VIAT0061) BRIEFD                                 *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVAT0061)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIAT0061 TABLE                                      
CCC004*    ( BRIEF                       CHAR(16) NOT NULL                      
CCC004*    , MA_ST_NR                    INTEGER NOT NULL                       
CCC004*    , KTY_DRUCK_STATUS            SMALLINT NOT NULL                      
CCC004*    , KNR_DRUCK_STATUS            SMALLINT NOT NULL                      
CCC004*    , BRIEFD_DAT_ERST             DATE NOT NULL                          
CCC004*    , BRIEFD_DAT_DRUCK            DATE NOT NULL                          
CCC004*    , BRIEFD_DAT_BRIEF            DATE NOT NULL                          
CCC004*    , BRIEFD_ONDEMAND             CHAR(01) NOT NULL                      
CCC004*    , BRIEFD_LADON                CHAR(01) NOT NULL                      
CCC004*    , BRIEFD_DRUCK                CHAR(01) NOT NULL                      
CCC004*    , BRIEFD_EMAIL_VB             CHAR(80)                               
CCC004*    , BRIEFD_EMAIL_FREI           CHAR(80)                               
CCC004*    , BRIEFD_OWNER                CHAR(40)                               
CCC004*    , BRIEFD_DRU_IP               CHAR(32)                               
CCC004*    , BRIEFD_DRU_NAME             CHAR(16)                               
CCC004*    , BRIEFD_FORM_DEF             CHAR(16)                               
CCC004*    , BRIEFD_FORMS                CHAR(16)                               
CCC004*    , BRIEFD_GESELL               CHAR(15)                               
CCC004*    , BRIEFD_BEZ                  CHAR(80)                               
CCC004*    , BRIEFD_LAST_UPD             TIMESTAMP NOT NULL                     
CCC004*    , BRIEFD_SIB                  VARCHAR(32000)                         
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIAT0061                          *         
      *****************************************************************         
                                                                                
       01  VIAT0061-GESAMT.                                                     
        05 NI-VIAT0061-TAB.                                                     
           10 NI-VIAT0061-ELE      PIC S9(04)  BINARY OCCURS  21.               
        05 NI-VIAT0061             REDEFINES   NI-VIAT0061-TAB.                 
           10 NI-BRIEF                    PIC S9(04)       BINARY.              
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.              
           10 NI-KTY-DRUCK-STATUS         PIC S9(04)       BINARY.              
           10 NI-KNR-DRUCK-STATUS         PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DAT-ERST          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DAT-DRUCK         PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DAT-BRIEF         PIC S9(04)       BINARY.              
           10 NI-BRIEFD-ONDEMAND          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-LADON             PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DRUCK             PIC S9(04)       BINARY.              
           10 NI-BRIEFD-EMAIL-VB          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-EMAIL-FREI        PIC S9(04)       BINARY.              
           10 NI-BRIEFD-OWNER             PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DRU-IP            PIC S9(04)       BINARY.              
           10 NI-BRIEFD-DRU-NAME          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-FORM-DEF          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-FORMS             PIC S9(04)       BINARY.              
           10 NI-BRIEFD-GESELL            PIC S9(04)       BINARY.              
           10 NI-BRIEFD-BEZ               PIC S9(04)       BINARY.              
           10 NI-BRIEFD-LAST-UPD          PIC S9(04)       BINARY.              
           10 NI-BRIEFD-SIB               PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIAT0061.                                                        
           10 BRIEF                       PIC  X(16).                           
           10 MA-ST-NR                    PIC S9(09)       BINARY.              
           10 KTY-DRUCK-STATUS            PIC S9(04)       BINARY.              
           10 KNR-DRUCK-STATUS            PIC S9(04)       BINARY.              
           10 BRIEFD-DAT-ERST             PIC  X(10).                           
           10 BRIEFD-DAT-DRUCK            PIC  X(10).                           
           10 BRIEFD-DAT-BRIEF            PIC  X(10).                           
           10 BRIEFD-ONDEMAND             PIC  X(01).                           
           10 BRIEFD-LADON                PIC  X(01).                           
           10 BRIEFD-DRUCK                PIC  X(01).                           
           10 BRIEFD-EMAIL-VB             PIC  X(80).                           
           10 BRIEFD-EMAIL-FREI           PIC  X(80).                           
           10 BRIEFD-OWNER                PIC  X(40).                           
           10 BRIEFD-DRU-IP               PIC  X(32).                           
           10 BRIEFD-DRU-NAME             PIC  X(16).                           
           10 BRIEFD-FORM-DEF             PIC  X(16).                           
           10 BRIEFD-FORMS                PIC  X(16).                           
           10 BRIEFD-GESELL               PIC  X(15).                           
           10 BRIEFD-BEZ                  PIC  X(80).                           
           10 BRIEFD-LAST-UPD             PIC  X(26).                           
           10 BRIEFD-SIB.                                                       
              49 BRIEFD-SIB-LAENGE        PIC S9(04)       BINARY.              
              49 BRIEFD-SIB-INHALT        PIC  X(32000).                        
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  21                    *         
      *****************************************************************