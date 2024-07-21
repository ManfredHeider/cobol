      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum     :  26.04.2013                              
      *            Kategorie :  Cobol-Programm                          
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DPAT0064.CPY  $                                 00001002

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DPAT0064.CPv  $                          
      *
      *   Rev 2.1   Jun 28 2013 14:57:50   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *                                                                 
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
           EVALUATE OPERATION IN EINAUS-:XX:                            
           WHEN  'ST '                                                  
             MOVE 1                                                     
               TO ZW-START-ERGEBNIS                                     
             MOVE GEFORDERTE-ANZAHL IN EINAUS-:XX:                      
               TO ZW-ENDE-ERGEBNIS                                      
             IF ZW-ENDE-ERGEBNIS > GESAMT-ANZAHL IN EINAUS-:XX:         
                MOVE GESAMT-ANZAHL IN EINAUS-:XX:                       
                  TO ZW-ENDE-ERGEBNIS                                   
             END-IF                                                     
           WHEN  'FW'                                                   
             COMPUTE ZW-START-ERGEBNIS =                                
                LETZTE-POSITION IN EINAUS-:XX: + 1                      
             END-COMPUTE                                                
             COMPUTE ZW-ENDE-ERGEBNIS = ZW-START-ERGEBNIS +             
                GEFORDERTE-ANZAHL IN EINAUS-:XX: - 1                    
             END-COMPUTE                                                
             IF ZW-ENDE-ERGEBNIS > GESAMT-ANZAHL IN EINAUS-:XX:         
                MOVE GESAMT-ANZAHL IN EINAUS-:XX:                       
                  TO ZW-ENDE-ERGEBNIS                                   
             END-IF                                                     
           WHEN  'BW'                                                   
             COMPUTE ZW-START-ERGEBNIS =                                
                LETZTE-POSITION   IN EINAUS-:XX: -                      
                GELIEFERTE-ANZAHL IN EINAUS-:XX: -                      
                GEFORDERTE-ANZAHL IN EINAUS-:XX: + 1                    
             END-COMPUTE                                                
             IF ZW-START-ERGEBNIS < 1                                   
                MOVE 1 TO ZW-START-ERGEBNIS                             
             END-IF                                                     
             COMPUTE ZW-ENDE-ERGEBNIS = ZW-START-ERGEBNIS +             
                GEFORDERTE-ANZAHL IN EINAUS-:XX: - 1                    
             END-COMPUTE                                                
             IF ZW-ENDE-ERGEBNIS > GESAMT-ANZAHL IN EINAUS-:XX:         
                MOVE GESAMT-ANZAHL IN EINAUS-:XX:                       
                  TO ZW-ENDE-ERGEBNIS                                   
             END-IF                                                     
           END-EVALUATE                                                 
      *                                                                 
           DIVIDE ZW-START-ERGEBNIS BY        KST-MAX-ZEILE GIVING      
                  ZW-START-SEQUENZ  REMAINDER ZW-START-ZEILE            
      *                                                                 
           IF ZW-START-ZEILE = ZERO                                     
              MOVE KST-MAX-ZEILE TO ZW-START-ZEILE                      
           ELSE                                                         
              COMPUTE ZW-START-SEQUENZ = ZW-START-SEQUENZ + 1           
              END-COMPUTE                                               
           END-IF                                                       
      *                                                                 
           DIVIDE ZW-ENDE-ERGEBNIS BY        KST-MAX-ZEILE GIVING       
                  ZW-ENDE-SEQUENZ  REMAINDER ZW-ENDE-ZEILE              
      *                                                                 
           IF ZW-ENDE-ZEILE = ZERO                                      
              MOVE KST-MAX-ZEILE TO ZW-ENDE-ZEILE                       
           ELSE                                                         
              COMPUTE ZW-ENDE-SEQUENZ = ZW-ENDE-SEQUENZ + 1             
              END-COMPUTE                                               
           END-IF                                                       
