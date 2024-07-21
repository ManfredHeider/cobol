      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  Januar 2001                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVBA078.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/CVVBA078.CPv  $                                    
      *
      *   Rev 2.1   23 Oct 2001 15:58:20   t13ae27
      *neu
      *                                                                         
      *   Rev 2.0   03 Sep 2001 10:55:12   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      ******************************************************************        
      * DCLGEN TABLE(T13AES.VIVBA078)                                  *        
      *        LIBRARY(T130005.AE010000.SRCDCL(CVVBA078))              *        
      *        LANGUAGE(COBOL)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVBA078 TABLE                                      
CCC004*    ( MA_ST_NR                       INTEGER NOT NULL,                   
CCC004*      KTY_WB_TYP                     SMALLINT NOT NULL,                  
CCC004*      KNR_WB_TYP                     SMALLINT NOT NULL,                  
CCC004*      KTY_WERTUNGS_TYP               SMALLINT NOT NULL,                  
CCC004*      KNR_WERTUNGS_TYP               SMALLINT NOT NULL,                  
CCC004*      KAND_WERTUNGSSTUFE             SMALLINT NOT NULL,                  
CCC004*      KAND_LAST_UPDATE               TIMESTAMP NOT NULL,                 
CCC004*      KAND_TAETIG_BIS                DATE NOT NULL,                      
CCC004*      KAND_DRUCK_KZ                  SMALLINT NOT NULL,                  
CCC004*      KAND_NEU_AB                    DATE,                               
CCC004*      KAND_RUECKERM                  CHAR                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE T13AES.VIVBA078                    *        
      ******************************************************************        
       01  VIVBA078-GESAMT.                                                     
        05  NI-VIVBA078-TAB.                                                    
           10 NI-VIVBA078-ELE      PIC S9(04) BINARY OCCURS 11  TIMES.          
        05  NI-VIVBA078            REDEFINES   NI-VIVBA078-TAB.                 
           10 NI-MA-ST-NR               PIC S9(04)       BINARY.                
           10 NI-KTY-WB-TYP             PIC S9(04)       BINARY.                
           10 NI-KNR-WB-TYP             PIC S9(04)       BINARY.                
           10 NI-KTY-WERTUNGS-TYP       PIC S9(04)       BINARY.                
           10 NI-KNR-WERTUNGS-TYP       PIC S9(04)       BINARY.                
           10 NI-KAND-WERTUNGSSTUFE     PIC S9(04)       BINARY.                
           10 NI-KAND-LAST-UPDATE       PIC S9(04)       BINARY.                
           10 NI-KAND-TAETIG-BIS        PIC S9(04)       BINARY.                
           10 NI-KAND-DRUCK-KZ          PIC S9(04)       BINARY.                
           10 NI-KAND-NEU-AB            PIC S9(04)       BINARY.                
           10 NI-KAND-RUECKERM          PIC S9(04)       BINARY.                
        05 DCL-VIVBA078.                                                        
           10 MA-ST-NR                  PIC S9(9)        BINARY.                
           10 KTY-WB-TYP                PIC S9(4)        BINARY.                
           10 KNR-WB-TYP                PIC S9(4)        BINARY.                
           10 KTY-WERTUNGS-TYP          PIC S9(4)        BINARY.                
           10 KNR-WERTUNGS-TYP          PIC S9(4)        BINARY.                
           10 KAND-WERTUNGSSTUFE        PIC S9(4)        BINARY.                
           10 KAND-LAST-UPDATE          PIC X(26).                              
           10 KAND-TAETIG-BIS           PIC X(10).                              
           10 KAND-DRUCK-KZ             PIC S9(4)        BINARY.                
           10 KAND-NEU-AB               PIC X(010).                             
           10 KAND-RUECKERM             PIC X(001).                             
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *        
      ******************************************************************        
