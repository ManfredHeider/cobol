      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  21.09.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   WVVB0317.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/WVVB0317.CPv  $
      *
      *   Rev 2.1   Oct 12 2012 11:28:44   t13ae36
      *AAS01038: Erfolge im Gruppengeschäft - Jahresanfangsbestände
      *
      *   Rev 2.0   Sep 21 2012 10:43:22   t13ae36
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ***************************************************************** 
      *    COPY-MEMBER: WVVB0317                                      * 
      *    --------------------------------------------------------   * 
      *    WORKBEREICH FÜR VIVB0317 (UEBJSEG )                        * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: -                                                * 
      *    DATUM:    21.09.2012                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                  * 
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      * 
      * --------+--------+----+---------------------------+-----------* 
      * AAS01038!21.09.12!2.1 ! NEU                       ! -         * 
      *         !        !    !                           !           * 
      *         !        !    !                           !           * 
      * ENDE-LOGBUCH                                                  * 
      ***************************************************************** 
      * COBOL DEKLARATION FÜR VIEW VIVB0317                           * 
      ***************************************************************** 
                                                                        
        05 WB-NI-VIVB0317-TAB.                                          
           10 WB-NI-VIVB0317-ELE   PIC S9(04)  BINARY OCCURS  10.       
        05 WB-NI-VIVB0317          REDEFINES   WB-NI-VIVB0317-TAB.      
           10 WB-NI-UEBJSEG-BETR-NR       PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-UNTST-NR      PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-MONAT         PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BEITRAG-AK    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BEITRAG-AL    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BESTUEB-HI    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BESTUEB-WE    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BEITRAG-SU    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-BEITRAG-DI    PIC S9(04)       BINARY.      
           10 WB-NI-UEBJSEG-FAKT-BS       PIC S9(04)       BINARY.      
                                                                        
        05 WB-VIVB0317.                                                 
           10 WB-UEBJSEG-BETR-NR          PIC S9(09)       BINARY.      
           10 WB-UEBJSEG-UNTST-NR         PIC S9(09)       BINARY.      
           10 WB-UEBJSEG-MONAT            PIC  X(10).                   
           10 WB-UEBJSEG-BEITRAG-AK       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-BEITRAG-AL       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-BESTUEB-HI       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-BESTUEB-WE       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-BEITRAG-SU       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-BEITRAG-DI       PIC S9(09)V9(02) COMP-3.      
           10 WB-UEBJSEG-FAKT-BS          PIC S9(05)V9(02) COMP-3.      
                                                                        
      ***************************************************************** 
      * ANZAHL SPALTEN IN DIESER DEKLARATION :  10                    * 
      ***************************************************************** 
