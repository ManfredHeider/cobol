      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVB0084.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/CVVB0084.CPv  $                                    
      *
      *   Rev 2.2   Jul 10 2002 14:32:48   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *                                                                         
      *   Rev 2.0   20 Feb 2001 13:22:22   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ******************************************************************        
      * DCLGEN TABLE(VIVB0084)                                         *        
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0084))              *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVB0084 TABLE                                      
CCC004*    (                                                                    
CCC004*       MA_ST_NR              INT         NOT NULL              ,
CCC004*       MURLZ_DAT_VON         TIMESTAMP   NOT NULL              ,
CCC004*       MURLZ_SST_KZ          CHAR (01)   NOT NULL  WITH DEFAULT,
CCC004*       MURLZ_LAST_UPDATE     TIMESTAMP   NOT NULL  WITH DEFAULT,
CCC004*       MURLZ_SST_DAT         DATE                              ,
CCC004*       MURLZ_DAT_BIS         DATE        NOT NULL  WITH DEFAULT,
CCC004*       MURLZ_URL             CHAR (70)   NOT NULL
CCC004*    )                                                                    
CCC003*    END-EXEC.                                                            
                                                                                
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0084                           *        
      ******************************************************************        
       01  VIVB0084-GESAMT.                                                     
           05  NI-VIVB0084-TAB.                                                 
               10 NI-VIVB0084-ELE      PIC S9(04) BINARY OCCURS 7 TIMES.
           05  NI-VIVB0084            REDEFINES   NI-VIVB0084-TAB.              
               10 NI-MA-ST-NR               PIC S9(04)  COMP.                   
               10 NI-MURLZ-DAT-VON          PIC S9(04)  COMP.                   
               10 NI-MURLZ-SST-KZ           PIC S9(04)  COMP.
               10 NI-MURLZ-LAST-UPDATE      PIC S9(04)  COMP.                   
               10 NI-MURLZ-SST-DAT          PIC S9(04)  COMP.
               10 NI-MURLZ-DAT-BIS          PIC S9(04)  COMP.
               10 NI-MURLZ-URL              PIC S9(04)  COMP.
           05 DCL-VIVB0084.                                                     
               10 MA-ST-NR                  PIC S9(9)   COMP.                   
               10 MURLZ-DAT-VON             PIC X(26).                          
               10 MURLZ-SST-KZ              PIC X(01).
               10 MURLZ-LAST-UPDATE         PIC X(26).
               10 MURLZ-SST-DAT             PIC X(10).
               10 MURLZ-DAT-BIS             PIC X(10).
               10 MURLZ-URL                 PIC X(70).
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************        
