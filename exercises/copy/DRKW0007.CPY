      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  08.06.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0007.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0007.CPv  $                                
      *
      *   Rev 2.1   Nov 11 2010 11:14:22   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   Jun 08 2010 10:54:38   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ******************************************************************
      *              #####################                             *
      *     ########    D R K W 0 0 0 7    ########                    *
      *              #####################                             *
      *                                                                *
      *     Copy-Strecke zur Aufbereitung von Fehlermeldungen in  das  *
      *     allgemeine Fehlerprotokoll (nach Erstellung z.Zt. nur f?r  *
      *     das KI-Projekt                                             *
      *                                                                *
      *     AUF VIELFACHEN WUNSCH EINES PROGRAMMIERERS WIRD DAS FELD   *
      *     FEHLER-FILLER WIEDER AUFGENOMMEN, DADURCH „NDERT SICH DIE  *
      *     SATZL„NGE AUF 150                                          *
      *                                                                *
      *                                                                *
      *    SATZLŽNGE   150                                             *
      *                                                                *
      *----------------------------------------------------------------*
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00966!07.05.10! 01 ! Neu                       ! Misgeld  *  
      *         !        !    ! Kopie von DRKI0561, jedoch!          *  
      *         !        !    ! mit Gesellschaft u. VNR   !          *  
      *         !        !    ! als Key                   !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       01  DRKW0007.                                                    
                                                                        
      *                                FEHLER-DATUM IM DB2-FORMAT       
      *                                (JHJJ-MM-TT) EINSTELLEN:         
           05  FEHLER-DATUM            PIC  X(010).                     
                                                                        
      *                                AUSGABE FEHLERHAFTES FELD        
      *                                EINGEFšGT                        
           05  FEHLER-FEHLERFELD REDEFINES FEHLER-DATUM PIC X(010).     
                                                                        
      *                                Fehler-Uhrzeit im DB2-Format     
      *                                (HH:MM:SS) einstellen:           
           05  FEHLER-PGM-BEZEICHNUNG     PIC  X(024).                  
                                                                        
      *                                Programmname des den Fehler      
      *                                erzeugenden Programms:           
                                                                        
           05  FEHLER-PGMNAME             PIC  X(008).                  
                                                                        
           05  FEHLER-KEY.                                              
      *                                Gesellschafts-ID:                
               10  FEHLER-GESELLSCHAFT    PIC  9(002).                  
                                                                        
      *                                Vertrags-Nummer:                 
               10  FEHLER-VERTRAGSNR      PIC  9(015).                  
                                                                        
      *                                Interne Vertrags-Nummer:         
           05  FEHLER-VERTRAGSNR-INTERN   PIC  9(007).                  
                                                                        
      *                                Fehlernummer und Fehlertext      
      *                                sind gem„á den Eintragungen      
      *                                in Copy-Strecke    DRKI0132      
      *                                einzutragen:                     
           05  FEHLER-FEHLERNUMMER        PIC  X(004).                  
                                                                        
      *                                Fehlertext bitte ohne die ersten 
      *                                   5 Stellen (= Fehlernummer)    
      *                                eintragen:                       
           05  FEHLER-FEHLERTEXT          PIC  X(045).                  
      *                                Teitltyp-Nummer:                 
           05  FEHLER-TEILTYP             PIC  9(002).                  
      *                                Teilschicht-Nummer:              
           05  FEHLER-TEILSCHICHT         PIC  9(003).                  
      *                                                                 
           05  FEHLER-ZUSATZINFO          PIC  X(015).                  
           05  FILLER                     PIC  X(015).                  
                                                                        
      ******************************************************************
      *          E n d e    Copy-Buch   DRKW0007                       *
      ******************************************************************
