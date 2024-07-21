      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVB0089.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/CVVB0089.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:08:38   t13ae35
      *ASL00153 - Zeichen aus Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:17:26   t13ae35
      *ASL00153 - neu erstellt
      *                                                                         
      *   Rev 2.0   Nov 15 2002 14:55:14   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *    ERSTELLT: J.Hering                                         * 00060002
      *    DATUM:    27.11.2002                                       * 00070002
      ***************************************************************** 00080000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * ASL00153!27.11.02! 2.1! NEU                       ! J.Hering *  00140002
      *         !        !    !                           !          *  00143100
      *         !        !    !                           !          *  00144000
      *         !        !    !                           !          *  00145000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ******************************************************************        
      * DCLGEN TABLE(VIVB0089) BOZB                                    *        
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0089)               *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVB0089 TABLE                                      
CCC004*    ( BOZB_ST_NR                  INTEGER NOT NULL,                      
CCC004*      BOZB_STICHTAG               DATE NOT NULL,                         
CCC004*      BOZB_ANSPR_VOM              DATE NOT NULL,                         
CCC004*      BOZB_LAST_UPDATE            TIMESTAMP NOT NULL,                    
CCC004*      BOZB_MS_NR                  SMALLINT NOT NULL,                     
CCC004*      BOZB_12MON_UMSATZ           DECIMAL(10, 1) NOT NULL,               
CCC004*      BOZB_12MON_EQF              DECIMAL(5, 2) NOT NULL,                
CCC004*      BOZB_03MON_UMSATZ           DECIMAL(10, 1) NOT NULL,               
CCC004*      BOZB_EQF_ABZUG              DECIMAL(10, 1) NOT NULL,               
CCC004*      BOZB_FAKTOR                 DECIMAL(5, 2),                         
CCC004*      BOZB_ANZ_MONATE             SMALLINT                               
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0089                                    
      ******************************************************************        
       01  VIVB0089-GESAMT.                                                     
        05  NI-VIVB0089-TAB.                                                    
           10 NI-VIVB0089-ELE      PIC S9(04) BINARY OCCURS  11 TIMES.          
        05  NI-VIVB0089            REDEFINES   NI-VIVB0089-TAB.                 
           10 NI-BOZB-ST-NR               PIC S9(04)       BINARY.              
           10 NI-BOZB-STICHTAG            PIC S9(04)       BINARY.              
           10 NI-BOZB-ANSPR-VOM           PIC S9(04)       BINARY.              
           10 NI-BOZB-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-BOZB-MS-NR               PIC S9(04)       BINARY.              
           10 NI-BOZB-12MON-UMSATZ        PIC S9(04)       BINARY.              
           10 NI-BOZB-12MON-EQF           PIC S9(04)       BINARY.              
           10 NI-BOZB-03MON-UMSATZ        PIC S9(04)       BINARY.              
           10 NI-BOZB-EQF-ABZUG           PIC S9(04)       BINARY.              
           10 NI-BOZB-FAKTOR              PIC S9(04)       BINARY.              
           10 NI-BOZB-ANZ-MONATE          PIC S9(04)       BINARY.              
                                                                                
        05  DCL-VIVB0089.                                                       
           10 BOZB-ST-NR                  PIC S9(09)       BINARY.              
           10 BOZB-STICHTAG               PIC X(10).                            
           10 BOZB-ANSPR-VOM              PIC X(10).                            
           10 BOZB-LAST-UPDATE            PIC X(26).                            
           10 BOZB-MS-NR                  PIC S9(04)       BINARY.              
           10 BOZB-12MON-UMSATZ           PIC S9(09)V9     COMP-3.              
           10 BOZB-12MON-EQF              PIC S9(03)V99    COMP-3.              
           10 BOZB-03MON-UMSATZ           PIC S9(09)V9     COMP-3.              
           10 BOZB-EQF-ABZUG              PIC S9(09)V9     COMP-3.              
           10 BOZB-FAKTOR                 PIC S9(03)V99    COMP-3.              
           10 BOZB-ANZ-MONATE             PIC S9(04)       BINARY.              
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *        
      ******************************************************************        
