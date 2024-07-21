      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  22.06.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0009.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0009.CPv  $                                
      *
      *   Rev 2.1   Nov 11 2010 11:14:30   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   Jun 22 2010 08:43:04   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *                                                               * 
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   * 
      *     - Aktualisierungsdatei DVAG Wien allg. Vertragsdaten      * 
      *                                                               * 
      *    SATZLŽNGE   320                                            * 
      *                                                               * 
      *---------------------------------------------------------------* 
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00966!22.06.10! 01 ! Neu                       ! Misgeld  *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       01  DRKW0009.                                                    
                                                                        
           05  VS-AKTUAL-KZ               PIC  X(001).                  
      *        I = INSERT                                               
      *        U = UPDATE                                               
      *        D = DELETE                                               
                                                                        
           05  VS-GES                     PIC  9(002).                  
           05  VS-VNR                     PIC  9(015).                  
                                                                        
           05  VS-VBNR                    PIC  9(007).                  
           05  VS-VBNR-AENDKZ             PIC  X(001).                  
      *        'J' wenn Abschluss-Vermittler (Vermögensberater-Nr.)
      *            geändert
                                                                        
           05  VS-VERTRAGSBEGINN          PIC  X(010).                  
           05  VS-VERTRAGSBEGINN-AENDKZ   PIC  X(001).                  
      *        'J' wenn Vertragsbeginn geändert
                                                                        
           05  VS-VERTRAGSABLAUF          PIC  X(010).                  
           05  VS-VERTRAGSABLAUF-AENDKZ   PIC  X(001).                  
      *        'J' wenn Vertragsablauf geändert
                                                                        
           05  VS-BEITRAG-GESAMT          PIC  9(008)V99.               
           05  VS-BEITRAG-GESAMT-AENDKZ   PIC  X(001).                  
      *        'J' wenn Beitrag Gesamt geändert
                                                                        
           05  VS-ZAHLWEISE               PIC S9(001).                  
           05  VS-ZAHLWEISE-AENDKZ        PIC  X(001).                  
      *        'J' wenn Zahlweise geändert
                                                                        
           05  VS-ZAHLUNGSART             PIC S9(002).                  
           05  VS-ZAHLUNGSART-AENDKZ      PIC  X(001).                  
      *        'J' wenn Zahlungsart geändert
                                                                        
           05  VS-TTMMHAUPTFAELL          PIC  9(004).                  
           05  VS-TTMMHAUPTFAELL-AENDKZ   PIC  X(001).                  
      *        'J' wenn TTMMHAUPTFAELL geändert
                                                                        
           05  VS-URL                     PIC  X(050).                  
           05  VS-URL-AENDKZ              PIC  X(001).                  
      *        'J' wenn URL geändert
                                                                        
           05  VS-INT-VERTRNR             PIC  9(009).                  
           05  VS-INT-VERTRNR-AENDKZ      PIC  X(001).                  
      *        'J' wenn interne Vertragsnummer geändert
                                                                        
           05  VS-RUECK-DTA-KZ            PIC  9(002).                  
           05  VS-RUECK-DTA-KZ-AENDKZ     PIC  X(001).                  
      *        'J' wenn Rück-DTA-KZ geändert

           05 FILLER                      PIC X(185).
                                                                        
           05  VS-BUENDEL-KZ              PIC  X(001).
           05  VS-BUENDEL-KZ-AENDKZ       PIC  X(001).
      *        'J' wenn Bündel-KZ geändert
                                                                        
      ******************************************************************
      *          E n d e    Copy-Buch   DRKW0009                       *
      ******************************************************************
