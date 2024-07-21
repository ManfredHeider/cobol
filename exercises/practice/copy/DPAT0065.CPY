      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum     :  26.04.2013                              
      *            Kategorie :  Cobol-Programm                          
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DPAT0065.CPY  $                                 00001004

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DPAT0065.CPv  $                          
      *
      *   Rev 2.1   Jun 28 2013 14:58:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *                                                                 
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
           IF USERID            IN EINAUS-:XX: = SPACE AND              
              TRANSACTION-CODE  IN EINAUS-:XX: = SPACE                  
              MOVE 01100017     TO MELDUNGS-NR IN AUS-:XX:              
              MOVE +08          TO RC          IN AUS-:XX:              
              GO TO PLAUSI-STEUER-EXIT                                  
           END-IF                                                       
      *                                                                 
           IF GEFORDERTE-ANZAHL IN EINAUS-:XX: NOT NUMERIC OR           
              GELIEFERTE-ANZAHL IN EINAUS-:XX: NOT NUMERIC OR           
              GESAMT-ANZAHL     IN EINAUS-:XX: NOT NUMERIC OR           
              LETZTE-POSITION   IN EINAUS-:XX: NOT NUMERIC              
              MOVE 01100017     TO MELDUNGS-NR IN AUS-:XX:              
              MOVE +08          TO RC          IN AUS-:XX:              
              GO TO PLAUSI-STEUER-EXIT                                  
           END-IF                                                       
      *                                                                 
           IF GEFORDERTE-ANZAHL IN EINAUS-:XX: < 01 OR                  
              GEFORDERTE-ANZAHL IN EINAUS-:XX: > KST-MAX-GEFORD-ANZ     
              MOVE 01100017     TO MELDUNGS-NR IN AUS-:XX:              
              MOVE +08          TO RC          IN AUS-:XX:              
              GO TO PLAUSI-STEUER-EXIT                                  
           END-IF                                                       
      *                                                                 
           EVALUATE OPERATION IN EINAUS-:XX:                            
           WHEN 'FB '                                                   
             IF LETZTE-POSITION   IN EINAUS-:XX: >=                     
                GESAMT-ANZAHL     IN EINAUS-:XX:                        
                MOVE 01110010     TO MELDUNGS-NR IN AUS-:XX:            
                MOVE +04          TO RC          IN AUS-:XX:            
                GO TO PLAUSI-STEUER-EXIT                                
             END-IF                                                     
           WHEN 'FW '                                                   
             IF LETZTE-POSITION   IN EINAUS-:XX: >=                     
                GESAMT-ANZAHL     IN EINAUS-:XX:                        
                MOVE 01110010     TO MELDUNGS-NR IN AUS-:XX:            
                MOVE +04          TO RC          IN AUS-:XX:            
                GO TO PLAUSI-STEUER-EXIT                                
             END-IF                                                     
           WHEN 'BW '                                                   
             IF LETZTE-POSITION   IN EINAUS-:XX: -                      
                GELIEFERTE-ANZAHL IN EINAUS-:XX: < 1                    
                MOVE 01110011     TO MELDUNGS-NR IN AUS-:XX:            
                MOVE +04          TO RC          IN AUS-:XX:            
                GO TO PLAUSI-STEUER-EXIT                                
             END-IF                                                     
           WHEN 'EB '                                                   
             CONTINUE                                                   
           WHEN 'SB '                                                   
             CONTINUE                                                   
           WHEN 'ST '                                                   
             CONTINUE                                                   
           WHEN OTHER                                                   
             MOVE 01100017        TO MELDUNGS-NR IN AUS-:XX:            
             MOVE +08             TO RC          IN AUS-:XX:            
             GO TO PLAUSI-STEUER-EXIT                                   
           END-EVALUATE                                                 
