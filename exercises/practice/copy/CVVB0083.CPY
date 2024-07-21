      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001                                       
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright 2001 ATLAS Dienstleistungs GmbH                            
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVB0083.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/MF/CPY/CVVB0083.CPv  $                                    
      *
      *   Rev 2.2   Jul 10 2002 14:32:46   t13ae30
      *AVB00587 - VB-EMAIL / VB-HOMEPAGE
      *                                                                         
      *   Rev 2.0   20 Feb 2001 13:22:22   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ******************************************************************        
      * DCLGEN TABLE(VIVB0083)                                         *        
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0083))              *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVB0083 TABLE                                      
CCC004*    (                                                                    
CCC004*       MA_ST_NR              INT         NOT NULL              ,         
CCC004*       MEMZ_DAT_VON          TIMESTAMP   NOT NULL              ,         
CCC004*       MEMZ_SST_KZ           CHAR (01)   NOT NULL  WITH DEFAULT,
CCC004*       MEMZ_LAST_UPDATE      TIMESTAMP   NOT NULL  WITH DEFAULT,
CCC004*       MEMZ_SST_DAT          DATE                              ,
CCC004*       MEMZ_DAT_BIS          DATE        NOT NULL  WITH DEFAULT,
CCC004*       MEMZ_EMAIL            CHAR (70)   NOT NULL
CCC004*    )                                                                    
CCC003*    END-EXEC.                                                            
                                                                                
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0083                           *        
      ******************************************************************        
       01  VIVB0083-GESAMT.                                                     
           05  NI-VIVB0083-TAB.                                                 
               10 NI-VIVB0083-ELE      PIC S9(04) BINARY OCCURS 7 TIMES.
           05  NI-VIVB0083            REDEFINES   NI-VIVB0083-TAB.              
               10 NI-MA-ST-NR               PIC S9(04)  COMP.                   
               10 NI-MEMZ-DAT-VON           PIC S9(04)  COMP.                   
               10 NI-MEMZ-SST-KZ            PIC S9(04)  COMP.
               10 NI-MEMZ-LAST-UPDATE       PIC S9(04)  COMP.
               10 NI-MEMZ-SST-DAT           PIC S9(04)  COMP.
               10 NI-MEMZ-DAT-BIS           PIC S9(04)  COMP.                   
               10 NI-MEMZ-EMAIL             PIC S9(04)  COMP.
           05 DCL-VIVB0083.                                                     
               10 MA-ST-NR                  PIC S9(9)   COMP.                   
               10 MEMZ-DAT-VON              PIC X(26).
               10 MEMZ-SST-KZ               PIC X(01).
               10 MEMZ-LAST-UPDATE          PIC X(26).
               10 MEMZ-SST-DAT              PIC X(10).
               10 MEMZ-DAT-BIS              PIC X(10).
               10 MEMZ-EMAIL                PIC X(70).
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 7       *
      ******************************************************************        
