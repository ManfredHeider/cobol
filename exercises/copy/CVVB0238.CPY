      *****************************************************************         
      *    ERSTELLT: KLEEMEIER                                        *         
      *    DATUM:    05.08.2022                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * JIRA     ! DATUM  ! AENDERUNG                     ! NAME      *         
      * ---------+--------+-------------------------------+---------- *         
      * VLA-5133 !05.08.22! NEU                           ! KLEEMEIER *         
      *          !        !                               !           *         
      *          !        !                               !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * DCLGEN TABLE(VIVB0238) BZLE                                   *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0238)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0238 TABLE                                      
CCC004*    ( BZLE_LFDNR                  SMALLINT NOT NULL                      
CCC004*    , KTY_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , KNR_GESELL_TYP              SMALLINT NOT NULL                      
CCC004*    , BZLE_ART                    CHAR(001) NOT NULL                     
CCC004*    , MA_ST_NR                    INTEGER NOT NULL                       
CCC004*    , BZLE_ERM_KZ                 CHAR(002) NOT NULL                     
CCC004*    , BZLE_MS_NR                  SMALLINT NOT NULL                      
CCC004*    , BZLE_VBP_NR                 SMALLINT                               
CCC004*    , BZLE_VBP_NR_G               SMALLINT NOT NULL                      
CCC004*    , BZLE_ZV_NR                  CHAR(001)                              
CCC004*    , BZLE_ZV_DAT_BIS             DATE                                   
CCC004*    , BZLE_ZV_DAT_VBG             DATE                                   
CCC004*    , BZLE_EH_EG_NEU              DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_EH_EG_NEU_A            DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_ERF_KZ_EG              CHAR(001) NOT NULL                     
CCC004*    , BZLE_JEQF_EG                DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLE_EH_EG_GES              DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_EH_EG_KORR             DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_EH_GR_GES              DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_EH_GES                 DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_EH_GES_A               DECIMAL(10, 1) NOT NULL                
CCC004*    , BZLE_ERF_KZ_GS              CHAR(001) NOT NULL                     
CCC004*    , BZLE_JEQF_GES               DECIMAL(05, 2) NOT NULL                
CCC004*    , BZLE_PROV_VERD              DECIMAL(11, 2) NOT NULL                
CCC004*    , BZLE_VBG_ABSICH             DECIMAL(11, 2) NOT NULL                
CCC004*    , BZLE_CEN_VNR                CHAR(015)                              
CCC004*    , BZLE_CKV_NEU                CHAR(001) NOT NULL                     
CCC004*    , BZLE_ASCLUB_DAT             DATE                                   
CCC004*    , BZLE_ASCLUB_BL              CHAR(001) NOT NULL                     
CCC004*    , BZLE_BS_VORJ                CHAR(001) NOT NULL                     
CCC004*    , BZLE_INFO_KZ                CHAR(002) NOT NULL                     
CCC004*    , BZLE_VERTRAG                CHAR(001) NOT NULL                     
CCC004*    , BZLE_DAT_VOLLM              DATE                                   
CCC004*    , BZLE_LAST_UPDATE            TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0238                          *         
      *****************************************************************         
                                                                                
       01 VIVB0238-GESAMT.                                                      
          05 NI-VIVB0238-TAB.                                                   
             10 NI-VIVB0238-ELE    PIC S9(004) BINARY OCCURS  34.               
          05 NI-VIVB0238           REDEFINES   NI-VIVB0238-TAB.                 
             10 NI-BZLE-LFDNR            PIC S9(004) BINARY.                    
             10 NI-KTY-GESELL-TYP        PIC S9(004) BINARY.                    
             10 NI-KNR-GESELL-TYP        PIC S9(004) BINARY.                    
             10 NI-BZLE-ART              PIC S9(004) BINARY.                    
             10 NI-MA-ST-NR              PIC S9(004) BINARY.                    
             10 NI-BZLE-ERM-KZ           PIC S9(004) BINARY.                    
             10 NI-BZLE-MS-NR            PIC S9(004) BINARY.                    
             10 NI-BZLE-VBP-NR           PIC S9(004) BINARY.                    
             10 NI-BZLE-VBP-NR-G         PIC S9(004) BINARY.                    
             10 NI-BZLE-ZV-NR            PIC S9(004) BINARY.                    
             10 NI-BZLE-ZV-DAT-BIS       PIC S9(004) BINARY.                    
             10 NI-BZLE-ZV-DAT-VBG       PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-EG-NEU        PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-EG-NEU-A      PIC S9(004) BINARY.                    
             10 NI-BZLE-ERF-KZ-EG        PIC S9(004) BINARY.                    
             10 NI-BZLE-JEQF-EG          PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-EG-GES        PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-EG-KORR       PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-GR-GES        PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-GES           PIC S9(004) BINARY.                    
             10 NI-BZLE-EH-GES-A         PIC S9(004) BINARY.                    
             10 NI-BZLE-ERF-KZ-GS        PIC S9(004) BINARY.                    
             10 NI-BZLE-JEQF-GES         PIC S9(004) BINARY.                    
             10 NI-BZLE-PROV-VERD        PIC S9(004) BINARY.                    
             10 NI-BZLE-VBG-ABSICH       PIC S9(004) BINARY.                    
             10 NI-BZLE-CEN-VNR          PIC S9(004) BINARY.                    
             10 NI-BZLE-CKV-NEU          PIC S9(004) BINARY.                    
             10 NI-BZLE-ASCLUB-DAT       PIC S9(004) BINARY.                    
             10 NI-BZLE-ASCLUB-BL        PIC S9(004) BINARY.                    
             10 NI-BZLE-BS-VORJ          PIC S9(004) BINARY.                    
             10 NI-BZLE-INFO-KZ          PIC S9(004) BINARY.                    
             10 NI-BZLE-VERTRAG          PIC S9(004) BINARY.                    
             10 NI-BZLE-DAT-VOLLM        PIC S9(004) BINARY.                    
             10 NI-BZLE-LAST-UPDATE      PIC S9(004) BINARY.                    
                                                                                
          05 DCL-VIVB0238.                                                      
             10 BZLE-LFDNR               PIC S9(004) BINARY.                    
             10 KTY-GESELL-TYP           PIC S9(004) BINARY.                    
             10 KNR-GESELL-TYP           PIC S9(004) BINARY.                    
             10 BZLE-ART                 PIC  X(001).                           
             10 MA-ST-NR                 PIC S9(009) BINARY.                    
             10 BZLE-ERM-KZ              PIC  X(002).                           
             10 BZLE-MS-NR               PIC S9(004) BINARY.                    
             10 BZLE-VBP-NR              PIC S9(004) BINARY.                    
             10 BZLE-VBP-NR-G            PIC S9(004) BINARY.                    
             10 BZLE-ZV-NR               PIC  X(001).                           
             10 BZLE-ZV-DAT-BIS          PIC  X(010).                           
             10 BZLE-ZV-DAT-VBG          PIC  X(010).                           
             10 BZLE-EH-EG-NEU           PIC S9(009)V9(001) COMP-3.             
             10 BZLE-EH-EG-NEU-A         PIC S9(009)V9(001) COMP-3.             
             10 BZLE-ERF-KZ-EG           PIC  X(001).                           
             10 BZLE-JEQF-EG             PIC S9(003)V9(002) COMP-3.             
             10 BZLE-EH-EG-GES           PIC S9(009)V9(001) COMP-3.             
             10 BZLE-EH-EG-KORR          PIC S9(009)V9(001) COMP-3.             
             10 BZLE-EH-GR-GES           PIC S9(009)V9(001) COMP-3.             
             10 BZLE-EH-GES              PIC S9(009)V9(001) COMP-3.             
             10 BZLE-EH-GES-A            PIC S9(009)V9(001) COMP-3.             
             10 BZLE-ERF-KZ-GS           PIC  X(001).                           
             10 BZLE-JEQF-GES            PIC S9(003)V9(002) COMP-3.             
             10 BZLE-PROV-VERD           PIC S9(009)V9(002) COMP-3.             
             10 BZLE-VBG-ABSICH          PIC S9(009)V9(002) COMP-3.             
             10 BZLE-CEN-VNR             PIC  X(015).                           
             10 BZLE-CKV-NEU             PIC  X(001).                           
             10 BZLE-ASCLUB-DAT          PIC  X(010).                           
             10 BZLE-ASCLUB-BL           PIC  X(001).                           
             10 BZLE-BS-VORJ             PIC  X(001).                           
             10 BZLE-INFO-KZ             PIC  X(002).                           
             10 BZLE-VERTRAG             PIC  X(001).                           
             10 BZLE-DAT-VOLLM           PIC  X(010).                           
             10 BZLE-LAST-UPDATE         PIC  X(026).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  34                    *         
      *****************************************************************         
