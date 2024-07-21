      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum     :  26.04.2013                              
      *            Kategorie :  Cobol-Programm                          
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DPAT0062.CPY  $                                 00001005

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DPAT0062.CPv  $                          
      *
      *   Rev 2.1   Jun 28 2013 14:57:30   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *                                                                 
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
           MOVE ZERO TO VZ-AKT-ZEILE                                    
           MOVE   1  TO VZ-AKT-SEQUENZ                                  
           MOVE ZERO TO ZW-ENDE-INDIKATOR                               
      *                                                                 
           PERFORM WITH TEST BEFORE                                     
                  UNTIL SQLSTATE IN SQLCA      = '02000'  OR            
                        ZW-ENDE-INDIKATOR      = 99       OR            
                        RC IN AUS-:XX: NOT = ZERO                       
              COMPUTE VZ-AKT-ZEILE = VZ-AKT-ZEILE + 1                   
              END-COMPUTE                                               
              PERFORM FUELLEN-SPEICHER                                  
              IF VZ-AKT-ZEILE = KST-MAX-ZEILE                           
                 PERFORM UPDATE-INSERT-SPEICHER                         
                 INITIALIZE DATENBEREICH IN WK1-SPEICHER                
                 MOVE ZERO TO VZ-AKT-ZEILE                              
                 COMPUTE VZ-AKT-SEQUENZ = VZ-AKT-SEQUENZ + 1            
                 END-COMPUTE                                            
                 IF VZ-AKT-SEQUENZ > KST-MAX-SEQUENZ                    
                    MOVE 99 TO ZW-ENDE-INDIKATOR                        
                 END-IF                                                 
              END-IF                                                    
              PERFORM FETCH-CURSOR                                      
           END-PERFORM                                                  
      *                                                                 
           IF RC IN AUS-:XX: NOT = ZERO                                 
              GO TO SPEICHERN-DATEN-EXIT                                
           END-IF                                                       
      *                                                                 
           IF SQLSTATE IN SQLCA = ZERO                                  
              MOVE 98 TO ZW-ENDE-INDIKATOR                              
           ELSE                                                         
              MOVE 99 TO ZW-ENDE-INDIKATOR                              
           END-IF                                                       
      *                                                                 
           COMPUTE GESAMT-ANZAHL IN EINAUS-:XX: =                       
              (VZ-AKT-SEQUENZ - 1) * KST-MAX-ZEILE + VZ-AKT-ZEILE       
           END-COMPUTE                                                  
      *                                                                 
           EVALUATE TRUE                                                
           WHEN VZ-AKT-SEQUENZ     = 1     AND                          
                VZ-AKT-ZEILE       = ZERO                               
             MOVE 01110009           TO MELDUNGS-NR IN AUS-:XX:         
             MOVE 'FET'              TO OPERATION   IN AUS-:XX:         
             MOVE SQLCA              TO AUS-SQLCA   IN AUS-:XX:         
             MOVE +04                TO RC          IN AUS-:XX:         
           WHEN VZ-AKT-SEQUENZ     > 1     AND                          
                VZ-AKT-ZEILE       = ZERO                               
             COMPUTE VZ-AKT-SEQUENZ = VZ-AKT-SEQUENZ - 1                
             END-COMPUTE                                                
             PERFORM LESEN-SPEICHER                                     
             IF RC IN AUS-:XX: NOT = ZERO                               
                GO TO SPEICHERN-DATEN-EXIT                              
             END-IF                                                     
             MOVE KST-MAX-ZEILE       TO VZ-AKT-ZEILE                   
             MOVE ZW-ENDE-INDIKATOR   TO ENDE-INDIKATOR IN WK1-HEADER   
             PERFORM UPDATE-INSERT-SPEICHER                             
           WHEN OTHER                                                   
             IF (GESAMT-ANZAHL        IN EINAUS-:XX: >                  
                 GEFORDERTE-ANZAHL    IN EINAUS-:XX: )      OR          
                (GESAMT-ANZAHL        IN EINAUS-:XX: >                  
                 KST-MAX-ZEILE                           )              
                MOVE ZW-ENDE-INDIKATOR TO ENDE-INDIKATOR IN WK1-HEADER  
                PERFORM UPDATE-INSERT-SPEICHER                          
             ELSE                                                       
                MOVE VZ-AKT-SEQUENZ    TO SEQUENZNUMMER  IN WK1-HEADER  
             END-IF                                                     
           END-EVALUATE                                                 
