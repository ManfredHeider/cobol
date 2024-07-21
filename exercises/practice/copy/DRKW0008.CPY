      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  22.06.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0008.CPY  $                                         


      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0008.CPv  $                            
      *
      *   Rev 2.2   May 21 2013 15:57:06   t13ex14
      *AKI01230: DVAG Wien: neues Feld Zession im Leben-DTA
      *
      *   Rev 2.1   Nov 11 2010 11:15:56   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   Jun 22 2010 08:42:56   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *                                                               * 
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   * 
      *     - Eingabedatei DVAG Wien Lebendaten mit Vorsatz           * 
      *                                                               * 
      *    SATZLŽNGE   700                                            * 
      *                                                               * 
      *---------------------------------------------------------------* 
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00966!07.07.10! 01 ! Neu                       ! Misgeld  *  
130513* AKI01230!13.05.13!    ! neues Feld ABTRETUNGKZ /  ! WMI      *
      *         !        !    ! ZESSION                   !          *
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       01  DRKW0008.                                                    
                                                                        
           05  DVW-LEB-VORSATZ.
               10 DVW-LEB-SATZART                     PIC  X.
      *           'A'
               10 DVW-LEB-ABSENDER                    PIC  X(005).
      *           'DWIEN'
               10 DVW-LEB-DTADATUM                    PIC  9(008).
      *           JJJJMMTT  (alle DTAs von einem Tag m?ssen in
      *           diesem Feld den gleichen Wert haben)
               10 DVW-LEB-EMPFAENGER                  PIC  X(004).
      *           'DVAG'
               10 DVW-LEB-DATENTEXT                   PIC  X(025).
      *           'LEBEN'
               10 DVW-LEB-VERABKZ                     PIC  X(004).
      *           'INIT'
               10 FILLER                              PIC  X(653).
                                                                        
           05  DVW-LEB-VERTRAGSDATEN REDEFINES DVW-LEB-VORSATZ.
                                                                        
               10 DVW-LEB-SATZART                     PIC  X.           
      *           'D'                                                   
               10 DVW-LEB-KEY.                                          
                  15 DVW-LEB-GESELLSCHAFT             PIC  9(002).      
      *              Produktgeber                                       
                  15 DVW-LEB-VERTRAGSNUMMER           PIC  9(015).      
      *                                                                 
               10 DVW-LEB-PRODUKT                      PIC  9(002).
      *                                                                 
               10 DVW-LEB-TARIF                       PIC  X(015).      
      *                                                                 
               10 DVW-LEB-PRZAHLENDE                  PIC  9(008).      
      *           Ende der Pr„mienzahlung (JJJJMMTT)                    
               10 DVW-LEB-VERS-SUMME                  PIC  9(011)V99.   
      *           Versicherungssumme                                    
               10 DVW-LEB-MIND-RISIKO-SUMME           PIC  9(011)V99.   
      *           Mindestrisikosumme (Todesfallsumme)                   
               10 DVW-LEB-BEI-ZUST                    PIC  9(002).      
      *           Beitragszustand                                       
      *           01 = BEITRAGSPFLICHTIG                                
      *           02 = BEITRAGSFREI                                     
      *           03 = GESTUNDET                                        
      *           04 = TEILGESTUNDET                                    
      *           05 = beitragsfrei wg. Invalidit„t                     
      *           06 = beitragsfrei wg. Ablauf Beitragszahler           
      *           07 = beitragsfrei wg. Todesfall                       
      *           08 = beitragsfrei wg. K?ndigung                       
      *           09 = beitragsfrei wg. Einmalzahlung                   
      *           10 = beitragsfrei wg. Sonderzahlung                   
      *           11 = ZULAGENGESPEIST                                  
      *           99 = UNBEKANNT                                        
               10 DVW-LEB-DIREKT-VERS-KZ              PIC  X(002).      
      *           BETRIEBSVERSICHERUNG                                  
      *           00 = keine Direktversicherung                         
      *           09 = GEHALTSUMWANDLUNGS-DIREKTVERSICHERUNG            
      *           69 = ARBEITGEBER FINANZIERTE DIREKTVERSICHERUNG       
               10 DVW-LEB-ZAHLWEISE-RENTE             PIC  9.           
      *           Rentenzahlweise bzw. Zahlweise der Ablaufleistung     
      *           0 = UNBEKANNTE ZAHLWEISE                              
      *           1 = JŽHRLICHE ZAHLWEISE                               
      *           2 = HALBJŽHRLICHE ZAHLWEISE                           
      *           4 = VIERTELJŽHRLICHE ZAHLWEISE                        
      *           6 = MONATLICHE ZAHLWEISE                              
      *           8 = EINMALZAHLUNG                                     
               10 DVW-LEB-ABLAUFRENTE                 PIC  9(011)V99.   
      *                                                                 
               10 DVW-LEB-GARANTIEZEIT                PIC  9(008).      
      *                                                                 
               10 DVW-LEB-GESAMTBEITRAG               PIC  9(011)V99.   
      *           Jahresbruttopr„mie                                    
               10 DVW-LEB-RATENBEITRAG                PIC  9(011)V99.   
      *           Bruttopr„mie lt. Zahlwesie                            
               10 DVW-LEB-ABLAUF                      PIC  9(011)V99.   
      *           Ablaufleistung                                        
               10 DVW-LEB-DEPBETR                     PIC  9(011)V99.   
      *           Fondswert                                             
               10 DVW-LEB-BUZRENTELEIST               PIC  9(011)V99.   
      *           BUZ-Rentenleistung                                    
               10 DVW-LEB-GARANTERLLEIST              PIC  9(011)V99.   
      *           Garantierte Erlebensleistung                          
               10 DVW-LEB-TAZTERM     OCCURS 9 TIMES  PIC  9(008).      
      *           Teilauszahlungstermin 1 - 9                           
               10 DVW-LEB-TAZRENTE    OCCURS 9 TIMES  PIC  9(011)V99.   
      *           Garantierte einmalige Teilauszahlung bzw.             
      *           Garantierte j„hrliche Teilpension  1 bis 9            
               10 DVW-LEB-TAZKAPITAL  OCCURS 9 TIMES  PIC  9(011)V99.   
      *                                                                 
               10 DVW-LEB-DYNAMIK                       PIC  9(002).
      *
               10 DVW-LEB-RUECKKAUFWERT                 PIC  9(011)V99.
      *
               10 DVW-LEB-NETTOGESBEITRAG               PIC 9(011)V99.
      *           Jahresnettoprämie
130513         10 DVW-LEB-ABTRETUNGKZ                   PIC X(001).
130513*           DVAG Wien-Terminus ZESSION J/N
130513*        10 FILLER                                PIC  X(193).
130513         10 FILLER                                PIC  X(192).

           05  DVW-LEB-NACHSATZ REDEFINES DVW-LEB-VORSATZ.
               10 DVW-LEB-SATZART                     PIC  X.
      *           'E'
               10 DVW-LEB-ANZ                         PIC  9(009).
      *           Satzanzahl incl. Vor- und Nachsatz
               10 FILLER                              PIC  X(690).
                                                                        
      ******************************************************************
      *          E n d e    Copy-Member DRKW0008                       *
      ******************************************************************
