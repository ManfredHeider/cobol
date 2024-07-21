      *---------------------------------------------------------------* 
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM                     
      *            DATUM      :  26.07.2007                             
      *            KATEGORIE  :  COPYSTRECKE                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPYRIGHT (C) 2007 ATLAS DIENSTLEISTUNGEN F. V. GMBH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2054.CPY  $                                 
      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2054.CPv  $                            
      *
      *   Rev 2.2   Dec 14 2010 09:50:44   t13ex15
      *AKI01005: Änderung AS-Club-Bedingungen (Investment)
      *
      *   Rev 2.1   Aug 06 2007 15:06:34   t13ex50
      *AKI00668: Beitragsvolumen für den Kundenleistungszuschuss (A
      *s-Club)
      *                                                                 
      *   REV 2.0   JUL 26 2007 07:51:12   T13EX50                      
      *ARCHIVERSTELLUNG                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRKI2054                                     *  
      *                                                              *  
      *    FUNKTION: COPY FUER EIN- UND AUSGABESATZ SIEHE UNTEN      *  
      *    -------------------------------------------------------   *  
      *    ERSTELLT: P. SCHLEICHER                                   *  
      *    DATUM:    26.07.2007                                      *  
      ****************************************************************  
      *    VERSION   01                                              *  
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00668!26.07.07! 01 !NEU                        !SCHLEICHER*  
      * -------------------------------------------------------------*
      * AKI01005!08.12.10!02  !Augabefelder INV, Topzins  !S. Warz   *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      * COBOLCOPY FÜR DIE EINLESESATZART AUS DER BEITRAGSVOLUMENDATEI   
      *   ...KI065000(GDG)                                              
      *     SATZLAENGE: 42 STELLEN                                      
      *     FORMAT:     F                                               
      *---------------------------------------------------------------* 
      *    FELDER AUS GUTHABENBUCHUNG                                   
      *         - VBNR / 7-STELLIGE BERATERNUMMER                       
      *         - VNID / KÜNSTLICHER SCHLÜSSEL PERSONEN-ID              
      *         - NUMMER DER PARTNERGESELLSCHAFT                        
      *         - (EXTERNE) VERTRAGSNUMMER                              
      *         - FOLGENUMMER                                           
      *         - PRAEMIENBETRAG FÜR DEN OBIGEN (DIESEN) SCHLÜSSEL      
      *---------------------------------------------------------------* 
       01  'XXXX'-SATZ.                                                 
           05 'XX'-KEY.                                                 
              07 'XX'-VERGLEICHSBEREICH.                                
                 10 'XX'-VBNR                 PIC  9(007)               
                                                    PACKED-DECIMAL.     
                 10 'XX'-VNID                 PIC  X(013).              
                 10 'XX'-GESELLSCHAFT         PIC S9(004)  BINARY.      
                 10 'XX'-VERTRAGSNUMMER       PIC  X(015).              
              07 'XX'-FOLGENUMMER          PIC  9(002).                 
           05 'XX'-PRAEMIE                 PIC S9(009)V9(002)           
                                                   PACKED-DECIMAL.      
081210     05 'XX'-PRAEMIE-INVEST          PIC S9(009)V9(002)
081210                                             PACKED-DECIMAL.
081210     05 'XX'-PRAEMIE-TOPZINS         PIC S9(009)V9(002)
081210                                             PACKED-DECIMAL.
      ***************************************************************** 
