      *****************************************************************         
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
      * DCLGEN TABLE(VIVB0146) KONZ                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0146)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0146 TABLE                                      
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL                       
CCC004*    , KTY_KONZ_TYP                SMALLINT NOT NULL                      
CCC004*    , KNR_KONZ_TYP                SMALLINT NOT NULL                      
CCC004*    , KONZ_ERF_DAT                DATE NOT NULL                          
CCC004*    , KONZ_LFD_NR                 SMALLINT NOT NULL                      
CCC004*    , KONZ_USER_ID                CHAR(08) NOT NULL                      
CCC004*    , KTY_STATUS_TYP              SMALLINT NOT NULL                      
CCC004*    , KNR_STATUS_TYP              SMALLINT NOT NULL                      
CCC004*    , KONZ_DATEN_ALT              CHAR(40) NOT NULL                      
CCC004*    , KONZ_DATEN_NEU              CHAR(40) NOT NULL                      
CCC004*    , KONZ_LAST_UPD               TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0146                          *         
      *****************************************************************         
                                                                                
       01  VIVB0146-GESAMT.                                                     
        05 NI-VIVB0146-TAB.                                                     
           10 NI-VIVB0146-ELE      PIC S9(04)  BINARY OCCURS  11.               
        05 NI-VIVB0146             REDEFINES   NI-VIVB0146-TAB.                 
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.              
           10 NI-KTY-KONZ-TYP             PIC S9(04)       BINARY.              
           10 NI-KNR-KONZ-TYP             PIC S9(04)       BINARY.              
           10 NI-KONZ-ERF-DAT             PIC S9(04)       BINARY.              
           10 NI-KONZ-LFD-NR              PIC S9(04)       BINARY.              
           10 NI-KONZ-USER-ID             PIC S9(04)       BINARY.              
           10 NI-KTY-STATUS-TYP           PIC S9(04)       BINARY.              
           10 NI-KNR-STATUS-TYP           PIC S9(04)       BINARY.              
           10 NI-KONZ-DATEN-ALT           PIC S9(04)       BINARY.              
           10 NI-KONZ-DATEN-NEU           PIC S9(04)       BINARY.              
           10 NI-KONZ-LAST-UPD            PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVB0146.                                                        
           10 MA-ST-NR                    PIC S9(09)       BINARY.              
           10 KTY-KONZ-TYP                PIC S9(04)       BINARY.              
           10 KNR-KONZ-TYP                PIC S9(04)       BINARY.              
           10 KONZ-ERF-DAT                PIC  X(10).                           
           10 KONZ-LFD-NR                 PIC S9(04)       BINARY.              
           10 KONZ-USER-ID                PIC  X(08).                           
           10 KTY-STATUS-TYP              PIC S9(04)       BINARY.              
           10 KNR-STATUS-TYP              PIC S9(04)       BINARY.              
           10 KONZ-DATEN-ALT              PIC  X(40).                           
           10 KONZ-DATEN-NEU              PIC  X(40).                           
           10 KONZ-LAST-UPD               PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  11                    *         
      *****************************************************************         
