      *---------------------------------------------------------------*         
      *    Standard für alle Objekte unter PVCS                                 
      *            Datum   :  November 2002                                     
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT                            
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2002 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   WVVB0088.CPY  $                                         
      *    $Revision:   2.2  $                                                  
      *                                                                         
      *    $Log:   L:/mf/cpy/WVVB0088.CPv  $
      *
      *   Rev 2.2   Dec 10 2002 16:11:38   t13ae35
      *ASL00153 - Zeichen aus Spalte 84 entfernt
      *
      *   Rev 2.1   Dec 10 2002 15:15:56   t13ae35
      *ASL00153 - neu erstellt
      *                                                                         
      *   Rev 2.0   Nov 15 2002 14:52:46   p13sp16                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      ***************************************************************** 00010000
      *    COPY-MEMBER: WVVB0088                                      * 00020002
      *    --------------------------------------------------------   * 00030000
      *    WORKBEREICH FÜR VIVB0088 (BOZA)                            * 00040002
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
      * COBOL DECLARATION FOR TABLE VIVB0088                           *        
      ******************************************************************        
        05  WB-NI-VIVB0088-TAB.                                                 
           10 WB-NI-VIVB0088-ELE   PIC S9(04)  BINARY OCCURS  10 TIMES.         
        05  WB-NI-VIVB0088         REDEFINES   WB-NI-VIVB0088-TAB.              
           10 WB-NI-BOZA-ST-NR            PIC S9(04)       BINARY.              
           10 WB-NI-BOZA-ANSPR-VOM        PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ANSPR-GRUND       PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ANSPR-GRUND       PIC S9(04)       BINARY.              
           10 WB-NI-BOZA-LAST-UPDATE      PIC S9(04)       BINARY.              
           10 WB-NI-KTY-WAEHRUNG          PIC S9(04)       BINARY.              
           10 WB-NI-KNR-WAEHRUNG          PIC S9(04)       BINARY.              
           10 WB-NI-BOZA-ANSPR-BETRAG     PIC S9(04)       BINARY.              
           10 WB-NI-KTY-ANSPR-STATUS      PIC S9(04)       BINARY.              
           10 WB-NI-KNR-ANSPR-STATUS      PIC S9(04)       BINARY.              
                                                                                
        05  WB-VIVB0088.                                                        
           10 WB-BOZA-ST-NR                  PIC S9(09)       BINARY.           
           10 WB-BOZA-ANSPR-VOM              PIC X(10).                         
           10 WB-KNR-ANSPR-GRUND             PIC S9(04)       BINARY.           
           10 WB-KTY-ANSPR-GRUND             PIC S9(04)       BINARY.           
           10 WB-BOZA-LAST-UPDATE            PIC X(26).                         
           10 WB-KTY-WAEHRUNG                PIC S9(04)       BINARY.           
           10 WB-KNR-WAEHRUNG                PIC S9(04)       BINARY.           
           10 WB-BOZA-ANSPR-BETRAG           PIC S9(09)V9(02) COMP-3.           
           10 WB-KTY-ANSPR-STATUS            PIC S9(04)       BINARY.           
           10 WB-KNR-ANSPR-STATUS            PIC S9(04)       BINARY.           
      ******************************************************************        
      * THE NUMBER OF COLUMNS DESCRIBED BY THIS DECLARATION IS 10      *        
      ******************************************************************        
