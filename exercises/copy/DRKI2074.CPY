      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  13.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2074.CPY  $                                 00010000

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2074.CPv  $                                
      *
      *   Rev 2.1   Oct 12 2007 09:53:00   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *                                                                 
      *   Rev 2.0   Sep 13 2007 14:56:00   t13ex50                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRKI2074                                     *  
      *                                                              *  
      *    FUNKTION: COPY FUER AUSGABESATZ SIEHE UNTEN               *  
      *    -------------------------------------------------------   *  
      *    ERSTELLT: P. SCHLEICHER                                   *  
      *    DATUM:    13.09.2007                                      *  
      ****************************************************************  
      *    VERSION   01                                              *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00630!13.09.07! 01 !NEU                        !SCHLEICHER*  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      * COBOLCOPY FÜR DIE FEHLERAUSGABE DER AS-CLUB-VERARBEITUNG        
      *                   ERWEITERTE STANDARDCOPY                       
      *     SATZLAENGE: 48 STELLEN                                      
      *     FORMAT:     F                                               
      *---------------------------------------------------------------* 
      *    FELDER DER SATZART                                           
      *         - VERTRAGS-ID / KÜNSTLICHER SCHLÜSSEL                   
      *         - (EXTERNE) VERTRAGSNUMMER                              
      *         - FILLER 1 STELLE                                       
      *         - NUMMER DER PARTNERGESELLSCHAFT                        
      *         - PRAEMIENBETRAG FÜR DEN OBIGEN (DIESEN) SCHLÜSSEL      
      *         - FILLER 1 STELLE                                       
      *         - SCHLUESSEL ZUR DEFININION DER ZAHLUNGSWEISE           
      *         - FILLER 1 STELLE                                       
      *         - SCHLUESSEL ZUR DEFININION DES VERTRAGSZUSTANDES       
      *         - FILLER 1 STELLE                                       
      *         - WÄHRUNGSKENNZEICHEN                                   
      *         - KENNZEICHEN GEBUENDELTER VERTRAG (J/N)                
      *         - KENNZEICHEN, OB VERTRAG EIGENVERMITTELT IST           
      *         - KENNZEICHEN, OB VERTRAG EIN FREMDVERTRAG IST (E/F/... 
      *---------------------------------------------------------------* 
       01  FEHLDAT-SATZ.                                                
           05  FVERVERTRAGSID                 PIC X(13).                
           05  FVERVERTRAGSNUMMER             PIC X(15).                
           05  FNIL-VERVERTRAGSNUMMER         PIC X(1).                 
           05  FVERPARTGESELLSCHID            PIC S9(4) COMP.           
           05  FVERGESAMTBEITRAG              PIC S9(9)V99 COMP-3.      
           05  FNIL-VERGESAMTBEITRAG          PIC X(01).                
           05  FVERZAHLUNGSWEISEID            PIC S9(4) COMP.           
           05  FNIL-VERZAHLUNGSWEISEID        PIC X(01).                
           05  FVERVERTRZUSTANDID             PIC S9(4) COMP.           
           05  FNIL-VERVERTRZUSTANDID         PIC X(01).                
           05  FVERGESBEITRWKZ                PIC X(01).                
           05  FVERGEBUENDELTKZ               PIC X(01).                
           05  FVEREIGENVERMKZ                PIC X(01).                
           05  FVERFREMDVERTRAGKZ             PIC X(01).                
