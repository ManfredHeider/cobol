      *****************************************************************         
      *    COPY-MEMBER: DRKI2435                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2435                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:06 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2435                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2435.                                                         
           05  PI-DRKI2435-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2435-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2435-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2435-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2435-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI2435.                                                         
           05  PO-DRKI2435-RC                     PIC  X(003).                  
               88 PO-DRKI2435-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2435-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI2435-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI2435-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI2435-GHAFLAGEINSTKURS   PIC  X(0001).                    
             10 PO-DRKI2435-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PO-DRKI2435-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI2435-GHAGUTHABENVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI2435-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PO-DRKI2435-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PO-DRKI2435-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PO-DRKI2435-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PO-DRKI2435-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI2435-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PO-DRKI2435-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI2435-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI2435-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI2435-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PO-DRKI2435-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PO-DRKI2435-GHALIEFERDATUM   PIC  X(010).                       
             10 PO-DRKI2435-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI2435-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
