      *---------------------------------------------------------------* 
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VM                     
      *            DATUM      :  13.09.2007                             
      *            KATEGORIE  :  COPYSTRECKE                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPYRIGHT (C) 2007 ATLAS DIENSTLEISTUNGEN F. V. GMBH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2072.CPY  $                                 
      *    $Revision:   2.3  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2072.CPv  $                            
      *
      *   Rev 2.3   Jul 02 2014 08:12:22   t13ex15
      *AKI01303: "VEP"-Wertung für AS-Club und KLZ Programme und Co
      *pys
      *
      *   Rev 2.2   Nov 30 2010 11:45:40   t13ex15
      *AKI01005: Änderung AS-Club-Bedingungen (Investment)
      *
      *   Rev 2.1   Oct 12 2007 09:52:48   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *                                                                 
      *   REV 2.0   SEP 13 2007 14:55:42   T13EX50                      
      *ARCHIVERSTELLUNG                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      *    COPY-MEMBER: DRKI2072                                     *  
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
      * -------------------------------------------------------------*
      * AKI01005!19.11.10!2.1 !2 neue Felder f.Invest     !S.Warz    *
      * -------------------------------------------------------------*
120614* AKI01303!12.06.14!2.2 !1 neues Feld f.AML VEP     !S.Warz    *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *                                                              *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
      * COBOLCOPY FÜR DIE AUSGABESATZART DER AS-CLUB-VERARBEITUNG       
      *   (MONATSLAUF) -> STANDARDCOPY                                  
      *     SATZLAENGE: 42 STELLEN                                      
      *     FORMAT:     F                                               
      *---------------------------------------------------------------* 
      *    FELDER AUS SATZART                                           
      *         - VBNR / 7-STELLIGE BERATERNUMMER                       
      *         - VNID / KÜNSTLICHER SCHLÜSSEL PERSONEN-ID              
      *         - NUMMER DER PARTNERGESELLSCHAFT                        
      *         - (EXTERNE) VERTRAGSNUMMER                              
      *         - FOLGENUMMER                                           
      *         - PRAEMIENBETRAG FÜR DEN OBIGEN (DIESEN) SCHLÜSSEL      
191110*         - PRAEMIENBETRAG FÜR INVEST GUTHABENANTEIL
191110*         - PRAEMIENBETRAG FÜR VERRECHNUNGSKONTO TOPZINS
      *---------------------------------------------------------------* 
                                                                        
       01  'XX'-FILE-SATZ.                                              
           05 'XX'-VBNR                        PIC 9(7) COMP-3.         
           05 'XX'-VNID                        PIC X(13).               
           05 'XX'-GES                         PIC S9(04) COMP.         
           05 'XX'-VERTRAGSNR                  PIC X(15).               
           05 'XX'-FOLGENR                     PIC 99.                  
           05 'XX'-PRAEMIE                     PIC S9(9)V99 COMP-3.     
191110     05 'XX'-PRAEMIE-GHA                 PIC S9(9)V99 COMP-3.
191110     05 'XX'-PRAEMIE-VKOTO-TOPZINS       PIC S9(9)V99 COMP-3.
120614     05 'XX'-PRAEMIE-AML-VEP             PIC S9(9)V99 COMP-3.
120614     05 FILLER                           PIC X(40).
