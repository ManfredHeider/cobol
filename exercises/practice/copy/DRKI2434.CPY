      *****************************************************************         
      *    COPY-MEMBER: DRKI2434                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2434                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:04 / U131774            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *          
      * -------------------------------------------------------------*          
      *         !        !    !                           !          *          
      *         !        !    !                           !          *          
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI2434                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2434.                                                         
           05  PI-DRKI2434-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2434-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2434-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2434-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2434-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2434-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI2434-NI-GHAKUMBARAUSBR   PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2434-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2434-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2434-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2434-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI2434-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2434-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI2434-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2434-GHAKUMBARAUSBRVJ   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2434-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2434-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2434-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2434-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2434-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2434-NI1.                                                     
           05 PI-DRKI2434-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI2434-NI7.                                                     
           05 PI-DRKI2434-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI2434.                                                         
           05  PO-DRKI2434-RC                     PIC  X(003).                  
               88 PO-DRKI2434-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2434-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
