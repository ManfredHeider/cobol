      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  06.05.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0002.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0002.CPv  $                                
      *
      *   Rev 2.1   Nov 11 2010 11:03:48   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   May 06 2010 12:12:50   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *                                                               * 
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   * 
      *     - Eingabedatei DVAG Wien Vertragsdaten                    * 
      *                                                               * 
      *    SATZLŽNGE   300                                            * 
      *---------------------------------------------------------------* 
      ****************************************************************  
      *    VERSION                                                      
      ****************************************************************  
      * AENDERUNGEN:                                                 *  
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *  
      * -------------------------------------------------------------*  
      * AKI00966!07.05.10! 01 ! Neu                       ! Misgeld  *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      *         !        !    !                           !          *  
      * ENDE-LOGBUCH                                                 *  
      ****************************************************************  
       01  DRKW0002.                                                    
                                                                        
           05  DVW-VALL-VORSATZ.                                        
                                                                        
               10 DVW-VOR-SATZART               PIC X(001).             
      *           'A'                                                   
               10 DVW-VALL-ABSENDER             PIC X(005).             
      *           'DWIEN'                                               
               10 DVW-VALL-DTADATUM             PIC 9(008).             
      *           JJJJMMTT  (alle DTAs von einem Tag m?ssen in          
      *           diesem Feld den gleichen Wert haben)                  
               10 DVW-VALL-EMPFAENGER           PIC X(004).             
      *           'DVAG'                                                
               10 DVW-VALL-DATENTEXT            PIC X(025).             
      *           'VERTRAG'                                             
               10 DVW-VALL-VERABKZ              PIC X(004).             
      *           'INIT'                                                
               10 FILLER                        PIC X(253).             
                                                                        
           05  DVW-VERTR-ALLGEMEIN REDEFINES DVW-VALL-VORSATZ.          
                                                                        
               10 DVW-VALL-SATZART              PIC X(001).             
      *           'D'                                                   
               10 DVW-VALL-KEY.                                         
                  15 DVW-VALL-GESELLSCHAFT      PIC 9(002).             
      *              Produktgeber                                       
                  15 DVW-VALL-VERTRAGSNUMMER    PIC 9(015).             
               10 DVW-VALL-VBNR                 PIC 9(007).             
      *           ohne Pr?fziffer                                       
               10 DVW-VALL-VBEGINN              PIC 9(008).             
      *           JJJJMMTT                                              
               10 DVW-VALL-VABLAUF              PIC 9(008).             
      *           JJJJMMTT oder 00000000 wenn nicht vorhanden           
               10 DVW-VALL-BEITRAG              PIC 9(008)V99.          
      *           Bruttobeitrag (incl. Ratenzahlungszuschlag            
      *           und Versicherungssteuer gem„á der Zahlweise)          
               10 DVW-VALL-ZAHLWEISE            PIC 9(001).             
      *           0  unbekannt                                          
      *           1  j„hrlich                                           
      *           2  halbj„hrlich                                       
      *           3  dreij„hrige                                        
      *           4  VIERTELJŽHRLICH                                    
      *           5  2-MONATLICH                                        
      *           6  MONATLICH                                          
      *           7  EINMALZ. LFZ MIN. 12 MON.                          
      *           8  EINMALZ./EINMALPRŽMIE                              
               10 DVW-VALL-ZAHLART              PIC 9(002).             
      *           1 = MASCH. ZAHLUNGSAUFFORDER                          
      *           5 = LASTSCHRIFT                                       
      *           99 = UNBEKANNT                                        
               10 DVW-VALL-TTMMHAUPTFAELL       PIC 9(004).             
      *           TTMM                                                  
               10 DVW-VALL-URL                  PIC X(050).             
      *           zur Verlinkung auf die Dim7-Seite                     
               10 DVW-VALL-INT-VERTRNR          PIC 9(009).             
      *           PIN ohne Pr?fziffer                                   
               10 DVW-VALL-RUECK-DTA-KZ         PIC 9(002).             
      *           KZ Generali Bestandssystem                            
               10 FILLER                        PIC X(180).
               10 DVW-VALL-BUENDEL-KZ           PIC x(001).
                                                                        
           05  DVW-VALL-NACHSATZ REDEFINES DVW-VALL-VORSATZ.            
                                                                        
               10 DVW-NACH-SATZART              PIC X(001).             
      *           'E'                                                   
               10 DVW-VALL-ANZ                  PIC 9(009).             
      *           Satzanzahl incl. Vor- und Nachsatz                    
               10 FILLER                       PIC X(290).              
                                                                        
      ******************************************************************
      *          E n d e    Copy-Member DRKW0002                       *
      ******************************************************************
