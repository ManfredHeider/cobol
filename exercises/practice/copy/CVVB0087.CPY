      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS VM                      
      *            Datum      :  14.03.2003                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   cvvb0087.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/CVVB0087.CPv  $                                
      *
      *   Rev 2.1   Apr 01 2003 17:51:50   t13ae35
      *AVB00618 - neu
      *                                                                 
      *   Rev 2.0   Mar 14 2003 10:10:40   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    ERSTELLT: J.Hering                                         * 
      *    DATUM:    17.03.2003                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00618!17.03.03! 2.1! NEU                       ! J.Hering *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ******************************************************************
      * DCLGEN TABLE(VIVB0087) FMIT                                    *
      *        LIBRARY(P130005.BP010000.SRCDCL(CVVB0087)               *
      *        ACTION(REPLACE)                                         *
      *        QUOTE                                                   *
      * ... IS THE DCLGEN COMMAND THAT MADE THE FOLLOWING STATEMENTS   *
      ******************************************************************
CCC005*    EXEC SQL DECLARE VIVB0087 TABLE                              
CCC004*    ( MA_ST_NR                    INTEGER NOT NULL,              
CCC004*      FMIT_DAT_VON                DATE NOT NULL,                 
CCC004*      KNR_FMIT_TYP                SMALLINT NOT NULL,             
CCC004*      KNR_WAEHRUNG                SMALLINT NOT NULL,             
CCC004*      FMIT_LAST_UPDATE            TIMESTAMP NOT NULL,            
CCC004*      KTY_FMIT_TYP                SMALLINT NOT NULL,             
CCC004*      KTY_WAEHRUNG                SMALLINT NOT NULL,             
CCC004*      FMIT_BETRAG                 DECIMAL(11, 2) NOT NULL        
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0087                            
      ******************************************************************
       01  VIVB0087-GESAMT.                                             
        05  NI-VIVB0087-TAB.                                            
           10 NI-VIVB0087-ELE      PIC S9(04) BINARY OCCURS  8 TIMES.   
        05  NI-VIVB0087            REDEFINES   NI-VIVB0087-TAB.         
           10 NI-MA-ST-NR                 PIC S9(04)       BINARY.      
           10 NI-FMIT-DAT-VON             PIC S9(04)       BINARY.      
           10 NI-KNR-FMIT-TYP             PIC S9(04)       BINARY.      
           10 NI-KNR-WAEHRUNG             PIC S9(04)       BINARY.      
           10 NI-FMIT-LAST-UPDATE         PIC S9(04)       BINARY.      
           10 NI-KTY-FMIT-TYP             PIC S9(04)       BINARY.      
           10 NI-KTY-WAEHRUNG             PIC S9(04)       BINARY.      
           10 NI-FMIT-BETRAG              PIC S9(04)       BINARY.      
                                                                        
        05  DCL-VIVB0087.                                               
           10 MA-ST-NR                    PIC S9(09)       BINARY.      
           10 FMIT-DAT-VON                PIC X(10).                    
           10 KNR-FMIT-TYP                PIC S9(04)       BINARY.      
           10 KNR-WAEHRUNG                PIC S9(04)       BINARY.      
           10 FMIT-LAST-UPDATE            PIC X(26).                    
           10 KTY-FMIT-TYP                PIC S9(04)       BINARY.      
           10 KTY-WAEHRUNG                PIC S9(04)       BINARY.      
           10 FMIT-BETRAG                 PIC S9(09)V9(02) USAGE COMP-3.
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 08      *
      * TUPLE-LENGTH = 70 BYTE                                         *
      ******************************************************************
