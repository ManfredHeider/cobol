      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  07.05.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0006.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0006.CPv  $                                
      *
      *   Rev 2.1   Nov 11 2010 11:14:14   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   May 07 2010 09:06:12   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *                                                               * 
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   * 
      *     - Aufbereitungs-Datei DVAG Wien Vertragsdaten             * 
      *                                                               * 
      *    SATZLŽNGE   320                                            * 
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
       01  DRKW0006.                                                    
                                                                        
           05  AUFB-VERTR-ALLGEMEIN.                                    
                                                                        
               10  AUFB-VALL-VERARB-KZ            PIC X(01).            
      *            Verarbeitungskennzeichen                             
      *            I = Insert U = Update                                
                                                                        
              10   AUFB-VALL-KEY.                                       
                   15 AUFB-VALL-GESELLSCHAFT      PIC 9(002).           
      *               Produktgeber                                      
                   15 AUFB-VALL-VERTRAGSNUMMER    PIC 9(015).           
                                                                        
               10 AUFB-VALL-VBNR                  PIC 9(007).           
      *           ohne Pr?fziffer                                       
               10 AUFB-VALL-VBNR-AEND-KZ          PIC X(001).           
                                                                        
               10 AUFB-VALL-VBEGINN               PIC 9(008).           
      *           JJJJMMTT                                              
               10 AUFB-VALL-VBEGINN-AEND-KZ       PIC X(001).           
                                                                        
               10 AUFB-VALL-VABLAUF               PIC 9(008).           
      *           JJJJMMTT oder 00000000 wenn nicht vorhanden           
               10 AUFB-VALL-VABLAUF-AEND-KZ       PIC X(001).           
                                                                        
               10 AUFB-VALL-BEITRAG               PIC 9(008)V99.        
      *           Bruttobeitrag (incl. Ratenzahlungszuschlag            
      *           und Versicherungssteuer gem„á der Zahlweise)          
               10 AUFB-VALL-BEITRAG-AEND-KZ       PIC X(001).           
                                                                        
               10 AUFB-VALL-ZAHLWEISE             PIC 9(001).           
      *           0  unbekannt                                          
      *           1  j„hrlich                                           
      *           2  halbj„hrlich                                       
      *           3  dreij„hrige                                        
      *           4  VIERTELJŽHRLICH                                    
      *           5  2-MONATLICH                                        
      *           6  MONATLICH                                          
      *           7  EINMALZ. LFZ MIN. 12 MON.                          
      *           8  EINMALZ./EINMALPRŽMIE                              
               10 AUFB-VALL-ZAHLWEISE-AEND-KZ     PIC X(001).           
                                                                        
               10 AUFB-VALL-ZAHLART               PIC 9(002).           
      *           1 = MASCH. ZAHLUNGSAUFFORDER                          
      *           5 = LASTSCHRIFT                                       
      *           99 = UNBEKANNT                                        
               10 AUFB-VALL-ZAHLART-AEND-KZ       PIC X(001).           
                                                                        
               10 AUFB-VALL-TTMMHAUPTFAELL        PIC 9(004).           
      *           TTMM                                                  
               10 AUFB-VALL-TTMMHAUPTFAE-AEND-KZ  PIC X(001).           
                                                                        
               10 AUFB-VALL-URL                   PIC X(050).           
      *           zur Verlinkung auf die Dim7-Seite                     
               10 AUFB-VALL-URL-AEND-KZ           PIC X(001).           
                                                                        
               10 AUFB-VALL-INT-VERTRNR           PIC 9(009).           
      *           PIN ohne Pr?fziffer                                   
               10 AUFB-VALL-INT-VERTRNR-AEND-KZ   PIC X(001).           
                                                                        
               10 AUFB-VALL-RUECK-DTA-KZ          PIC 9(002).           
      *           KZ Generali Bestandssystem                            
               10 AUFB-VALL-RUECK-DTA-KZ-AEND-KZ  PIC X(001).           

               10 FILLER                          PIC X(189).
                                                                        
               10 AUFB-VALL-BUENDEL-KZ            PIC x(001).
               10 AUFB-VALL-BUENDEL-KZ-AEND-KZ    PIC X(001).
                                                                        
      ******************************************************************
      *          E n d e    Copy-Member DRKW0006                       *
      ******************************************************************
