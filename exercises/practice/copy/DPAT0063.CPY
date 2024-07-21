      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum     :  26.04.2013                              
      *            Kategorie :  Cobol-Programm                          
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DPAT0063.CPY  $                                 00001004

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DPAT0063.CPv  $                          
      *
      *   Rev 2.1   Jun 28 2013 14:57:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *                                                                 
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
           PERFORM ERM-START-ENDE                                       
      *                                                                 
           IF RC IN AUS-:XX: NOT = ZERO                                 
             GO TO FUELLEN-STEUER-O-EXIT                                
           END-IF                                                       
           MOVE ZW-START-SEQUENZ   TO  VZ-AKT-SEQUENZ                   
           MOVE ZW-START-ZEILE     TO  VZ-AKT-ZEILE                     
           IF SEQUENZNUMMER IN WK1-HEADER NOT = VZ-AKT-SEQUENZ          
              PERFORM LESEN-SPEICHER                                    
           END-IF                                                       
           IF RC IN AUS-:XX: NOT = ZERO                                 
              GO TO FUELLEN-STEUER-O-EXIT                               
           END-IF                                                       
           MOVE ZERO TO GELIEFERTE-ANZAHL IN EINAUS-:XX:                
      *                                                                 
           PERFORM WITH TEST AFTER                                      
                VARYING LETZTE-POSITION IN EINAUS-:XX:                  
                   FROM ZW-START-ERGEBNIS                               
                     BY 1                                               
                  UNTIL LETZTE-POSITION IN EINAUS-:XX: =                
                        ZW-ENDE-ERGEBNIS                                
      *                                                                 
             COMPUTE GELIEFERTE-ANZAHL IN EINAUS-:XX: =                 
                     GELIEFERTE-ANZAHL IN EINAUS-:XX: + 1               
             END-COMPUTE                                                
      *                                                                 
             PERFORM UEBERTR-DATEN                                      
      *                                                                 
             IF RC IN AUS-:XX: NOT = ZERO                               
                GO TO FUELLEN-STEUER-O-EXIT                             
             END-IF                                                     
      *                                                                 
             IF VZ-AKT-ZEILE = KST-MAX-ZEILE                            
                IF ZW-ENDE-ERGEBNIS > LETZTE-POSITION IN EINAUS-:XX:    
                   COMPUTE VZ-AKT-SEQUENZ = VZ-AKT-SEQUENZ + 1          
                   END-COMPUTE                                          
                   MOVE   1  TO VZ-AKT-ZEILE                            
                   PERFORM LESEN-SPEICHER                               
                   IF RC IN AUS-:XX: NOT = ZERO                         
                      GO TO FUELLEN-STEUER-O-EXIT                       
                   END-IF                                               
                END-IF                                                  
             ELSE                                                       
                COMPUTE VZ-AKT-ZEILE = VZ-AKT-ZEILE + 1                 
                END-COMPUTE                                             
             END-IF                                                     
      *                                                                 
           END-PERFORM                                                  
