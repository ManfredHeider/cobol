      *---------------------------------------------------------------* 
      *    Standard für alle Objekte unter PVCS VM                      
      *            Datum      :  14.03.2003                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   wvvb0087.cpy  $                                 
      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/cpy/WVVB0087.CPv  $                                
      *
      *   Rev 2.1   Apr 01 2003 17:48:48   t13ae35
      *AVB00618 - neu, Workbereich für FMIT
      *                                                                 
      *   Rev 2.0   Mar 14 2003 10:11:06   p13sp16                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: WVVB0087                                      * 
      *    --------------------------------------------------------   * 
      *    WORKBEREICH FÜR VIVB0087 (FMIT)                            * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: J.Hering                                         * 
      *    DATUM:    27.11.2002                                       * 
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AVB00618!17.03.03! 2.1! NEU                       ! J.Hering *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE VIVB0087                           *
      ******************************************************************
        05  WB-NI-VIVB0087-TAB.                                         
           10 WB-NI-VIVB0087-ELE   PIC S9(04)  BINARY OCCURS   8 TIMES. 
        05  WB-NI-VIVB0087         REDEFINES   WB-NI-VIVB0087-TAB.      
           10 WB-NI-MA-ST-NR              PIC S9(04)       BINARY.      
           10 WB-NI-FMIT-DAT-VON          PIC S9(04)       BINARY.      
           10 WB-NI-KNR-FMIT-TYP          PIC S9(04)       BINARY.      
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.      
           10 WB-NI-FMIT-LAST-UPDATE      PIC S9(04)       BINARY.      
           10 WB-NI-KTY-FMIT-TYP          PIC S9(04)       BINARY.      
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.      
           10 WB-NI-FMIT-BETRAG           PIC S9(04)       BINARY.      
                                                                        
        05  WB-VIVB0087.                                                
           10 WB-MA-ST-NR                    PIC S9(09)       BINARY.   
           10 WB-FMIT-DAT-VON                PIC X(10).                 
           10 WB-KNR-FMIT-TYP                PIC S9(04)       BINARY.   
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.   
           10 WB-FMIT-LAST-UPDATE            PIC X(26).                 
           10 WB-KTY-FMIT-TYP                PIC S9(04)       BINARY.   
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.   
           10 WB-FMIT-BETRAG                 PIC S9(09)V9(02) COMP-3.   
      ******************************************************************
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 08      *
      * TUPLE-LENGTH = 70 BYTE                                         *
      ******************************************************************
