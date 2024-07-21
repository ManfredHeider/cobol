      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CVVB0088.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/CVVB0088.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:08:16   t13ae35
      *ASL00153 - Zeichen aus Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:17:10   t13ae35
      *ASL00153 - neu erstellt
      *                                                                         
      *   Rev 2.0   Nov 15 2002 14:52:20   p13sp16                              
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
      * DCLGEN TABLE(VIVB0088) BOZA                                    *        
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0088)               *        
      *        ACTION(REPLACE)                                         *        
      *        QUOTE                                                   *        
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *        
      ******************************************************************        
CCC005*    EXEC SQL DECLARE VIVB0088 TABLE                                      
CCC004*    ( BOZA_ST_NR                  INTEGER NOT NULL,                      
CCC004*      BOZA_ANSPR_VOM              DATE NOT NULL,                         
CCC004*      KNR_ANSPR_GRUND             SMALLINT NOT NULL,                     
CCC004*      KTY_ANSPR_GRUND             SMALLINT NOT NULL,                     
CCC004*      BOZA_LAST_UPDATE            TIMESTAMP NOT NULL,                    
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,                     
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,                     
CCC004*      BOZA_ANSPR_BETRAG           DECIMAL(11, 2) NOT NULL,               
CCC004*      KTY_ANSPR_STATUS            SMALLINT NOT NULL,                     
CCC004*      KNR_ANSPR_STATUS            SMALLINT NOT NULL                      
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0088                                    
      ******************************************************************        
       01  VIVB0088-GESAMT.                                                     
        05  NI-VIVB0088-TAB.                                                    
           10 NI-VIVB0088-ELE      PIC S9(04) BINARY OCCURS  10 TIMES.          
        05  NI-VIVB0088            REDEFINES   NI-VIVB0088-TAB.                 
           10 NI-BOZA-ST-NR               PIC S9(04)       BINARY.              
           10 NI-BOZA-ANSPR-VOM           PIC S9(04)       BINARY.              
           10 NI-KNR-ANSPR-GRUND          PIC S9(04)       BINARY.              
           10 NI-KTY-ANSPR-GRUND          PIC S9(04)       BINARY.              
           10 NI-BOZA-LAST-UPDATE         PIC S9(04)       BINARY.              
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.              
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.              
           10 NI-BOZA-ANSPR-BETRAG        PIC S9(04)       BINARY.              
           10 NI-KTY-ANSPR-STATUS         PIC S9(04)       BINARY.              
           10 NI-KNR-ANSPR-STATUS         PIC S9(04)       BINARY.              
                                                                                
        05  DCL-VIVB0088.                                                       
           10 BOZA-ST-NR                  PIC S9(09)       BINARY.              
           10 BOZA-ANSPR-VOM              PIC X(10).                            
           10 KNR-ANSPR-GRUND             PIC S9(04)       BINARY.              
           10 KTY-ANSPR-GRUND             PIC S9(04)       BINARY.              
           10 BOZA-LAST-UPDATE            PIC X(26).                            
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.              
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.              
           10 BOZA-ANSPR-BETRAG           PIC S9(09)V9(02) USAGE COMP-3.        
           10 KTY-ANSPR-STATUS            PIC S9(04)       BINARY.              
           10 KNR-ANSPR-STATUS            PIC S9(04)       BINARY.              
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *        
      ******************************************************************        
