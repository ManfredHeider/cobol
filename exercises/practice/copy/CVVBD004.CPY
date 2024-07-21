      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVBD004.cpy  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/CVVBD004.CPv  $                                    
      *
      *   Rev 2.2   Jan 14 2003 11:18:30   t13ae28
      *AVB00649: Dim 7ukunft - Neue Bef.Bed. RGS - D (2.)
      *
      *   Rev 2.1   Jan 14 2003 10:23:36   t13ae28
      *AVB00649: Dim 7uknft - Neue Bef.Bed. RGS-D
      *                                                                         
      *   Rev 2.0   Nov 26 2002 10:01:46   t13ex15                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      * AVB00649!26.11.02!4.0 ! Neu                       ! D.Ehrmann*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      ******************************************************************        
      * DCLGEN TABLE(VIVBD004)                                         *        
      *        LIBRARY(T130005.DB2PILOT.SRCDCL(CVVBD004))              *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVBD004 TABLE                                      
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,                   
CCC004*      MA_LAST_UPDATE                 TIMESTAMP NOT NULL,                 
CCC004*      KTY_MA_TYP                     SMALLINT NOT NULL,                  
CCC004*      KNR_MA_TYP                     SMALLINT NOT NULL,                  
CCC004*      MA_BDV_MITGLIED                CHAR(1) NOT NULL,                   
CCC004*      MA_STRAFE                      CHAR(1) NOT NULL,                   
CCC004*      MA_VERSANDSTOP                 CHAR(1) NOT NULL,                   
CCC004*      MA_ZV_ZVII_ABL                 CHAR(1) NOT NULL,                   
CCC004*      MA_RCK_STZ                     DECIMAL(5, 2) NOT NULL,             
CCC004*      MA_RCK_STZ_DAT                 DATE NOT NULL,                      
CCC004*      MA_FUEH_ZEUG_DAT               DATE,                               
CCC004*      MA_SCHUFA_DAT                  DATE,                               
CCC004*      MA_34C_DAT                     DATE,                               
CCC004*      MA_AFO                         CHAR(1) NOT NULL,                   
CCC004*      MA_DARLEHEN                    CHAR(1) NOT NULL,                   
CCC004*      MA_AVAG_DVAG                   DATE,                               
CCC004*      MA_KOMPASS_AVAG                DATE,                               
CCC004*      MA_AUTOTELEFON                 CHAR(14) NOT NULL,                  
CCC004*      MA_TELETEXT                    CHAR(14) NOT NULL,                  
CCC004*      MA_ZV_NR                       CHAR(1) NOT NULL,                   
CCC004*      MA_ZV_DAT_BIS_R                DATE,                               
CCC004*      MA_ZV_DAT_BIS                  DATE,                               
CCC004*      MA_ZV_DAT_VON                  DATE,                               
CCC004*      MA_ZV_KUE_DAT                  DATE,                               
CCC004*      MA_ED_ZUSATZ                   SMALLINT NOT NULL,                  
CCC004*      MA_ZV_VERL                     CHAR(1) NOT NULL,                   
CCC004*      MA_FG_DAT                      DATE,                               
CCC004*      MA_127_DAT                     DATE,                               
CCC004*      MA_VA_DAT                      DATE,                               
CCC004*      MA_ANWERBER                    INTEGER NOT NULL,                   
CCC004*      MA_BEFBED_NEU                  DATE                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVBD004                           *        
      ******************************************************************        
       01  VIVBD004-GESAMT.                                                     
        05  NI-VIVBD004-TAB.                                                    
           10  NI-VIVBD004-ELE     PIC S9(04)  COMP   OCCURS  31 TIMES.         
        05  NI-VIVBD004            REDEFINES   NI-VIVBD004-TAB.                 
           10 NI-MA-ST-NR          PIC S9(04)  COMP.                            
           10 NI-MA-LAST-UPDATE    PIC S9(04)  COMP.                            
           10 NI-KTY-MA-TYP        PIC S9(04)  COMP.                            
           10 NI-KNR-MA-TYP        PIC S9(04)  COMP.                            
           10 NI-MA-BDV-MITGLIED   PIC S9(04)  COMP.                            
           10 NI-MA-STRAFE         PIC S9(04)  COMP.                            
           10 NI-MA-VERSANDSTOP    PIC S9(04)  COMP.                            
           10 NI-MA-ZV-ZVII-ABL    PIC S9(04)  COMP.                            
           10 NI-MA-RCK-STZ        PIC S9(04)  COMP.                            
           10 NI-MA-RCK-STZ-DAT    PIC S9(04)  COMP.                            
           10 NI-MA-FUEH-ZEUG-DAT  PIC S9(04)  COMP.                            
           10 NI-MA-SCHUFA-DAT     PIC S9(04)  COMP.                            
           10 NI-MA-34C-DAT        PIC S9(04)  COMP.                            
           10 NI-MA-AFO            PIC S9(04)  COMP.                            
           10 NI-MA-DARLEHEN       PIC S9(04)  COMP.                            
           10 NI-MA-AVAG-DVAG      PIC S9(04)  COMP.                            
           10 NI-MA-KOMPASS-AVAG   PIC S9(04)  COMP.                            
           10 NI-MA-AUTOTELEFON    PIC S9(04)  COMP.                            
           10 NI-MA-TELETEXT       PIC S9(04)  COMP.                            
           10 NI-MA-ZV-NR          PIC S9(04)  COMP.                            
           10 NI-MA-ZV-DAT-BIS-R   PIC S9(04)  COMP.                            
           10 NI-MA-ZV-DAT-BIS     PIC S9(04)  COMP.                            
           10 NI-MA-ZV-DAT-VON     PIC S9(04)  COMP.                            
           10 NI-MA-ZV-KUE-DAT     PIC S9(04)  COMP.                            
           10 NI-MA-ED-ZUSATZ      PIC S9(04)  COMP.                            
           10 NI-MA-ZV-VERL        PIC S9(04)  COMP.                            
           10 NI-MA-FG-DAT         PIC S9(04)  COMP.                            
           10 NI-MA-127-DAT        PIC S9(04)  COMP.                            
           10 NI-MA-VA-DAT         PIC S9(04)  COMP.                            
           10 NI-MA-ANWERBER       PIC S9(04)  COMP.                            
           10 NI-MA-BEFBED-NEU     PIC S9(04)  COMP.
        05  DCL-VIVBD004.                                                       
           10 MA-ST-NR             PIC S9(9) USAGE COMP.                        
           10 MA-LAST-UPDATE       PIC X(26).                                   
           10 KTY-MA-TYP           PIC S9(4) USAGE COMP.                        
           10 KNR-MA-TYP           PIC S9(4) USAGE COMP.                        
           10 MA-BDV-MITGLIED      PIC X(1).                                    
           10 MA-STRAFE            PIC X(1).                                    
           10 MA-VERSANDSTOP       PIC X(1).                                    
           10 MA-ZV-ZVII-ABL       PIC X(1).                                    
           10 MA-RCK-STZ           PIC S999V99 USAGE COMP-3.                    
           10 MA-RCK-STZ-DAT       PIC X(10).                                   
           10 MA-FUEH-ZEUG-DAT     PIC X(10).                                   
           10 MA-SCHUFA-DAT        PIC X(10).                                   
           10 MA-34C-DAT           PIC X(10).                                   
           10 MA-AFO               PIC X(1).                                    
           10 MA-DARLEHEN          PIC X(1).                                    
           10 MA-AVAG-DVAG         PIC X(10).                                   
           10 MA-KOMPASS-AVAG      PIC X(10).                                   
           10 MA-AUTOTELEFON       PIC X(14).                                   
           10 MA-TELETEXT          PIC X(14).                                   
           10 MA-ZV-NR             PIC X(1).                                    
           10 MA-ZV-DAT-BIS-R      PIC X(10).                                   
           10 MA-ZV-DAT-BIS        PIC X(10).                                   
           10 MA-ZV-DAT-VON        PIC X(10).                                   
           10 MA-ZV-KUE-DAT        PIC X(10).                                   
           10 MA-ED-ZUSATZ         PIC S9(4) USAGE COMP.                        
           10 MA-ZV-VERL           PIC X(1).                                    
           10 MA-FG-DAT            PIC X(10).                                   
           10 MA-127-DAT           PIC X(10).                                   
           10 MA-VA-DAT            PIC X(10).                                   
           10 MA-ANWERBER          PIC S9(9) USAGE COMP.                        
           10 MA-BEFBED-NEU        PIC X(10).                                   
                                                                                
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 31      *        
      ******************************************************************        
