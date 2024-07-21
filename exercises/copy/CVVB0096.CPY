      *****************************************************************         
      *    ERSTELLT: T13AE30                                          *         
      *    DATUM:    05.03.2021                                       *         
      *****************************************************************         
      * AENDERUNGEN:                                                  *         
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *         
      * --------+--------+----+---------------------------+---------- *         
      * VLA-1995!05.03.21!2.1 ! NEU                       ! T13AE30   *         
      *         !        !    !                           !           *         
      *         !        !    !                           !           *         
      * ENDE-LOGBUCH                                                  *         
      *****************************************************************         
      * DCLGEN TABLE(VIVB0096) EBB                                    *         
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0096)              *         
      *        ACTION(REPLACE)                                        *         
      *        QUOTE                                                  *         
      * ... DIESES DCLGEN WURDE GENERIERT MIT GENERCBL (REXX)         *         
      *****************************************************************         
                                                                                
CCC005*    EXEC SQL DECLARE VIVB0096 TABLE                                      
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL                       
CCC004*    , KTY_TYP_BEFBED              SMALLINT NOT NULL                      
CCC004*    , KNR_TYP_BEFBED              SMALLINT NOT NULL                      
CCC004*    , LAUF_DAT                    DATE NOT NULL                          
CCC004*    , STF_AKT                     SMALLINT                               
CCC004*    , STF_SEIT                    DATE                                   
CCC004*    , ZPKT_VON                    DATE                                   
CCC004*    , ZPKT_BIS                    DATE                                   
CCC004*    , EH_EIG                      DECIMAL(10, 1)                         
CCC004*    , EH_EIG_KZ                   CHAR(01)                               
CCC004*    , EQF_EIG                     DECIMAL(05, 2)                         
CCC004*    , EQF_EIG_KZ                  CHAR(01)                               
CCC004*    , EH_GES                      DECIMAL(10, 1)                         
CCC004*    , KORR_EH_GES                 DECIMAL(10, 1)                         
CCC004*    , KORR_EH_GES_KZ              CHAR(01)                               
CCC004*    , EQF_GES                     DECIMAL(05, 2)                         
CCC004*    , EQF_GES_KZ                  CHAR(01)                               
CCC004*    , EH_NEU_PART                 DECIMAL(10, 1)                         
CCC004*    , EH_PART_KZ                  CHAR(01)                               
CCC004*    , PROZ_KLAUS                  DECIMAL(10, 1)                         
CCC004*    , EIGEN_MAX                   DECIMAL(10, 1)                         
CCC004*    , GESAMT_KZ                   CHAR(01) NOT NULL                      
CCC004*    , LAST_UPDATE                 TIMESTAMP NOT NULL                     
CCC003*    ) END-EXEC.                                                          
                                                                                
      *****************************************************************         
      * COBOL DEKLARATION FUER VIEW VIVB0096                          *         
      *****************************************************************         
                                                                                
       01  VIVB0096-GESAMT.                                                     
        05 NI-VIVB0096-TAB.                                                     
           10 NI-VIVB0096-ELE      PIC S9(04)  BINARY OCCURS  23.               
        05 NI-VIVB0096             REDEFINES   NI-VIVB0096-TAB.                 
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.              
           10 NI-KTY-TYP-BEFBED           PIC S9(04)       BINARY.              
           10 NI-KNR-TYP-BEFBED           PIC S9(04)       BINARY.              
           10 NI-LAUF-DAT                 PIC S9(04)       BINARY.              
           10 NI-STF-AKT                  PIC S9(04)       BINARY.              
           10 NI-STF-SEIT                 PIC S9(04)       BINARY.              
           10 NI-ZPKT-VON                 PIC S9(04)       BINARY.              
           10 NI-ZPKT-BIS                 PIC S9(04)       BINARY.              
           10 NI-EH-EIG                   PIC S9(04)       BINARY.              
           10 NI-EH-EIG-KZ                PIC S9(04)       BINARY.              
           10 NI-EQF-EIG                  PIC S9(04)       BINARY.              
           10 NI-EQF-EIG-KZ               PIC S9(04)       BINARY.              
           10 NI-EH-GES                   PIC S9(04)       BINARY.              
           10 NI-KORR-EH-GES              PIC S9(04)       BINARY.              
           10 NI-KORR-EH-GES-KZ           PIC S9(04)       BINARY.              
           10 NI-EQF-GES                  PIC S9(04)       BINARY.              
           10 NI-EQF-GES-KZ               PIC S9(04)       BINARY.              
           10 NI-EH-NEU-PART              PIC S9(04)       BINARY.              
           10 NI-EH-PART-KZ               PIC S9(04)       BINARY.              
           10 NI-PROZ-KLAUS               PIC S9(04)       BINARY.              
           10 NI-EIGEN-MAX                PIC S9(04)       BINARY.              
           10 NI-GESAMT-KZ                PIC S9(04)       BINARY.              
           10 NI-LAST-UPDATE              PIC S9(04)       BINARY.              
                                                                                
        05 DCL-VIVB0096.                                                        
           10 MA-ST-NR                    PIC S9(09)       BINARY.              
           10 KTY-TYP-BEFBED              PIC S9(04)       BINARY.              
           10 KNR-TYP-BEFBED              PIC S9(04)       BINARY.              
           10 LAUF-DAT                    PIC  X(10).                           
           10 STF-AKT                     PIC S9(04)       BINARY.              
           10 STF-SEIT                    PIC  X(10).                           
           10 ZPKT-VON                    PIC  X(10).                           
           10 ZPKT-BIS                    PIC  X(10).                           
           10 EH-EIG                      PIC S9(09)V9(01) COMP-3.              
           10 EH-EIG-KZ                   PIC  X(01).                           
           10 EQF-EIG                     PIC S9(03)V9(02) COMP-3.              
           10 EQF-EIG-KZ                  PIC  X(01).                           
           10 EH-GES                      PIC S9(09)V9(01) COMP-3.              
           10 KORR-EH-GES                 PIC S9(09)V9(01) COMP-3.              
           10 KORR-EH-GES-KZ              PIC  X(01).                           
           10 EQF-GES                     PIC S9(03)V9(02) COMP-3.              
           10 EQF-GES-KZ                  PIC  X(01).                           
           10 EH-NEU-PART                 PIC S9(09)V9(01) COMP-3.              
           10 EH-PART-KZ                  PIC  X(01).                           
           10 PROZ-KLAUS                  PIC S9(09)V9(01) COMP-3.              
           10 EIGEN-MAX                   PIC S9(09)V9(01) COMP-3.              
           10 GESAMT-KZ                   PIC  X(01).                           
           10 LAST-UPDATE                 PIC  X(26).                           
                                                                                
      *****************************************************************         
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  23                    *         
      *****************************************************************         
