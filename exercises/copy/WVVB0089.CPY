      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WVVB0089.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/WVVB0089.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:11:08   t13ae35
      *ASL00153 - Zeichen aus Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:16:24   t13ae35
      *ASL00153 - neu, erstellt
      *                                                                         
      *   Rev 2.0   Nov 15 2002 14:55:28   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0089                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    WORKBEREICH FÜR VIVB0089 (BOZB)                            * 00040002
      *    --------------------------------------------------------   * 00050000
      *    ERSTELLT: J.Hering                                         * 00060002
      *    DATUM:    27.11.2002                                       * 00070002
      ****************************************************************  00100000
      * AENDERUNGEN:                                                 *  00110000
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  00120000
      * -------------------------------------------------------------*  00130000
      * ASL00153!27.11.02! 2.1! NEU                       ! J.Hering *  00140002
      *         !        !    !                           !          *  00143000
      *                                                              *  00146000
      * ENDE-LOGBUCH                                                 *  00147000
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE VIVB0089                           *        
      ******************************************************************        
        05  WB-NI-VIVB0089-TAB.                                                 
           10 WB-NI-VIVB0089-ELE   PIC S9(04)  BINARY OCCURS  11 TIMES.         
        05  WB-NI-VIVB0089         REDEFINES   WB-NI-VIVB0089-TAB.              
           10 WB-NI-BOZB-ST-NR            PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-STICHTAG         PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-ANSPR-VOM        PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-MS-NR            PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-12MON-UMSATZ     PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-12MON-EQF        PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-03MON-UMSATZ     PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-EQF-ABZUG        PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-FAKTOR           PIC S9(04)       BINARY.              
           10 WB-NI-BOZB-ANZ-MONATE       PIC S9(04)       BINARY.              
                                                                                
        05  WB-VIVB0089.                                                        
           10 WB-BOZB-ST-NR                  PIC S9(09)       BINARY.           
           10 WB-BOZB-STICHTAG               PIC X(10).                         
           10 WB-BOZB-ANSPR-VOM              PIC X(10).                         
           10 WB-BOZB-LAST-UPDATE            PIC X(26).                         
           10 WB-BOZB-MS-NR                  PIC S9(04)       BINARY.           
           10 WB-BOZB-12MON-UMSATZ           PIC S9(09)V9     COMP-3.           
           10 WB-BOZB-12MON-EQF              PIC S9(03)V99    COMP-3.           
           10 WB-BOZB-03MON-UMSATZ           PIC S9(09)V9     COMP-3.           
           10 WB-BOZB-EQF-ABZUG              PIC S9(09)V9     COMP-3.           
           10 WB-BOZB-FAKTOR                 PIC S9(03)V99    COMP-3.           
           10 WB-BOZB-ANZ-MONATE             PIC S9(04)       BINARY.           
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 11      *        
      ******************************************************************        
