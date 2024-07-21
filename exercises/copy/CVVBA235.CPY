      *****************************************************************         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    06.01.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-3688 !06.01.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * DCLGEN TABLE(VIVBA235) FAPE                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVBA235)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVBA235 TABLE                                      
CCC004*    ( FAPE_LFDNR                  SMALLINT NOT NULL                      
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , FAPE_ART                    CHAR(01) NOT NULL                      
CCC004*    , MA_ST_NR                    INTEGER NOT NULL                       
CCC004*    , FAPE_ERM_KZ                 CHAR(02) NOT NULL                      
CCC004*    , FAPE_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC004*    , FAPE_MS_NR                  SMALLINT NOT NULL                      
CCC004*    , FAPE_VBP_NR                 SMALLINT NOT NULL                      
CCC004*    , FAPE_EH_EG_NEU              DECIMAL(10, 1)                         
CCC004*    , FAPE_EH_EG_GES              DECIMAL(10, 1)                         
CCC004*    , FAPE_EH_GES                 DECIMAL(10, 1)                         
CCC004*    , FAPE_EH_GES_KORR            DECIMAL(10, 1)                         
CCC004*    , FAPE_EH_GES_3MON            DECIMAL(10, 1)                         
CCC004*    , FAPE_JEQF_EG                DECIMAL(05, 2)                         
CCC004*    , FAPE_JEQF_GES               DECIMAL(05, 2)                         
CCC004*    , FAPE_BES                    SMALLINT                               
CCC004*    , FAPE_ALTERSGR_KZ            CHAR(01)                               
CCC004*    , FAPE_EINTR_KUEND            DATE                                   
CCC004*    , FAPE_MAT_DAT_BIS            DATE                                   
CCC004*    , FAPE_ERF_KZ_EG              CHAR(01) NOT NULL                      
CCC004*    , FAPE_ERF_KZ_GS              CHAR(01) NOT NULL                      
CCC004*    , FAPE_STUFE                  CHAR(01) NOT NULL                      
CCC004*    , FAPE_STUFE_DVAG             CHAR(01) NOT NULL                      
CCC004*    , FAPE_EH_EG_NEU_A            DECIMAL(10, 1) NOT NULL                
CCC004*    , FAPE_EH_GES_A               DECIMAL(10, 1) NOT NULL                
CCC004*    , FAPE_EH_GES_3M_A            DECIMAL(10, 1) NOT NULL                
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVBA235                          *         
      *****************************************************************         
                                                                                
       01  VIVBA235-GESAMT.                                                     
        05 NI-VIVBA235-TAB.                                                     
           10 NI-VIVBA235-ELE      PIC S9(04)  BINARY OCCURS  27.               
        05 NI-VIVBA235             REDEFINES   NI-VIVBA235-TAB.                 
           10 NI-FAPE-LFDNR               PIC S9(04)       BINARY.              
           10 NI-KTY-GESELL-TYP           PIC S9(04)       BINARY.              
           10 NI-KNR-GESELL-TYP           PIC S9(04)       BINARY.              
           10 NI-FAPE-ART                 PIC S9(04)       BINARY.              
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.              
           10 NI-FAPE-ERM-KZ              PIC S9(04)       BINARY.              
           10 NI-FAPE-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-FAPE-MS-NR               PIC S9(04)       BINARY.              
           10 NI-FAPE-VBP-NR              PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-EG-NEU           PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-EG-GES           PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-GES              PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-GES-KORR         PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-GES-3MON         PIC S9(04)       BINARY.              
           10 NI-FAPE-JEQF-EG             PIC S9(04)       BINARY.              
           10 NI-FAPE-JEQF-GES            PIC S9(04)       BINARY.              
           10 NI-FAPE-BES                 PIC S9(04)       BINARY.              
           10 NI-FAPE-ALTERSGR-KZ         PIC S9(04)       BINARY.              
           10 NI-FAPE-EINTR-KUEND         PIC S9(04)       BINARY.              
           10 NI-FAPE-MAT-DAT-BIS         PIC S9(04)       BINARY.              
           10 NI-FAPE-ERF-KZ-EG           PIC S9(04)       BINARY.              
           10 NI-FAPE-ERF-KZ-GS           PIC S9(04)       BINARY.              
           10 NI-FAPE-STUFE               PIC S9(04)       BINARY.              
           10 NI-FAPE-STUFE-DVAG          PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-EG-NEU-A         PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-GES-A            PIC S9(04)       BINARY.              
           10 NI-FAPE-EH-GES-3M-A         PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVBA235.                                                        
           10 FAPE-LFDNR                  PIC S9(04)       BINARY.              
           10 KTY-GESELL-TYP              PIC S9(04)       BINARY.              
           10 KNR-GESELL-TYP              PIC S9(04)       BINARY.              
           10 FAPE-ART                    PIC  X(01).                           
           10 MA-ST-NR                    PIC S9(09)       BINARY.              
           10 FAPE-ERM-KZ                 PIC  X(02).                           
           10 FAPE-LAST-UPDATE            PIC  X(26).                           
           10 FAPE-MS-NR                  PIC S9(04)       BINARY.              
           10 FAPE-VBP-NR                 PIC S9(04)       BINARY.              
           10 FAPE-EH-EG-NEU              PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-EG-GES              PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-GES                 PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-GES-KORR            PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-GES-3MON            PIC S9(09)V9(01) COMP-3.              
           10 FAPE-JEQF-EG                PIC S9(03)V9(02) COMP-3.              
           10 FAPE-JEQF-GES               PIC S9(03)V9(02) COMP-3.              
           10 FAPE-BES                    PIC S9(04)       BINARY.              
           10 FAPE-ALTERSGR-KZ            PIC  X(01).                           
           10 FAPE-EINTR-KUEND            PIC  X(10).                           
           10 FAPE-MAT-DAT-BIS            PIC  X(10).                           
           10 FAPE-ERF-KZ-EG              PIC  X(01).                           
           10 FAPE-ERF-KZ-GS              PIC  X(01).                           
           10 FAPE-STUFE                  PIC  X(01).                           
           10 FAPE-STUFE-DVAG             PIC  X(01).                           
           10 FAPE-EH-EG-NEU-A            PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-GES-A               PIC S9(09)V9(01) COMP-3.              
           10 FAPE-EH-GES-3M-A            PIC S9(09)V9(01) COMP-3.              
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  27                    *         
      *****************************************************************         
