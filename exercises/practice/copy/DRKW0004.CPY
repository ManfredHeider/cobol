      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  07.05.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKW0004.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKW0004.CPv  $                                
      *
      *   Rev 2.1   Nov 11 2010 11:14:00   t13ex14
      *AKI00966: DVAG Österreich: Kunden- und Vertrags-DTAs Aktuali
      *sierung
      *                                                                 
      *   Rev 2.0   May 07 2010 08:37:10   t13ex14                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *                                                               * 
      *    COPY-MEMBER ZU DEN ERMITTLUNGSPROGRAMMEN                   * 
      *     - KSDS-Datei DVAG Wien Vertragsdaten                      * 
      *                                                               * 
      *    SATZLŽNGE   340                                            * 
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
       01  DRKW0004.                                                    
                                                                        
                                                                        
           05  KOMPL-VALL-AEND                    PIC  9(002).          
                                                                        
           05  KOMPL-VALL-SATZ.                                         
               10 KOMPL-VALL-SATZART              PIC X(001).           
                                                                        
               10 KOMPL-VALL-KEY.                                       
                  15 KOMPL-VALL-GESELLSCHAFT      PIC 9(002).           
      *           Produktgeber                                          
                  15 KOMPL-VALL-VERTRAGSNUMMER    PIC 9(015).           
                                                                        
               10 KOMPL-VALL-VBNR                 PIC 9(007).           
      *           ohne Pr?fziffer                                       
               10 KOMPL-VALL-VBEGINN              PIC 9(008).           
      *           JJJJMMTT                                              
               10 KOMPL-VALL-VABLAUF              PIC 9(008).           
      *           JJJJMMTT oder 00000000 wenn nicht vorhanden           
               10 KOMPL-VALL-BEITRAG              PIC 9(008)V99.        
      *           Bruttobeitrag (incl. Ratenzahlungszuschlag            
      *           und Versicherungssteuer gem„á der Zahlweise)          
               10 KOMPL-VALL-ZAHLWEISE            PIC 9(001).           
      *           0  unbekannt                                          
      *           1  j„hrlich                                           
      *           2  halbj„hrlich                                       
      *           3  dreij„hrige                                        
      *           4  VIERTELJŽHRLICH                                    
      *           5  2-MONATLICH                                        
      *           6  MONATLICH                                          
      *           7  EINMALZ. LFZ MIN. 12 MON.                          
      *           8  EINMALZ./EINMALPRŽMIE                              
               10 KOMPL-VALL-ZAHLART              PIC 9(002).           
      *           1 = MASCH. ZAHLUNGSAUFFORDER                          
      *           5 = LASTSCHRIFT                                       
      *           99 = UNBEKANNT                                        
               10 KOMPL-VALL-TTMMHAUPTFAELL       PIC 9(004).           
      *           TTMM                                                  
               10 KOMPL-VALL-URL                  PIC X(050).           
      *           zur Verlinkung auf die Dim7-Seite                     
               10 KOMPL-VALL-INT-VERTRNR          PIC 9(009).           
      *           PIN ohne Pr?fziffer                                   
               10 KOMPL-VALL-RUECK-DTA-KZ         PIC 9(002).           
      *           KZ Generali Bestandssystem                            
               10 FILLER                          PIC X(180).
               10 KOMPL-VALL-BUENDEL-KZ           PIC x(001).
                                                                        
             05 KOMPL-VALL-TIMESTAMP              PIC X(026).           
      *         JJJJ-MM-TT-hh.mm.ss.abcdef                              
      *         abcdef = Millisekunden (wird mit 0 geliefert)           
             05 FILLER                            PIC X(012).           
      ******************************************************************
      *          E n d e    Copy-Member DRKW0004                       *
      ******************************************************************
