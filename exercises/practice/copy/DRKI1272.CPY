      *****************************************************************         
      *    COPY-MEMBER: DRKI1272                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1272                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:54 / U131774            
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
      *    FÜR DIE FUNKTION DRKI1272                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1272.                                                         
           05  PI-DRKI1272-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1272-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI1272-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI1272-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI1272-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI1272.                                                         
           05  PO-DRKI1272-RC                     PIC  X(003).                  
               88 PO-DRKI1272-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1272-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI1272-NI.                                                  
               10 NI-DRKI1272-TAB.                                              
                  15 DRKI1272-ELE                                               
                     PIC S9(04) COMP OCCURS 4 TIMES.                            
               10 NI-DRKI1272 REDEFINES NI-DRKI1272-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI1272-NI-GHAGUTHABEN   PIC S9(4) COMP.                    
             15 PO-DRKI1272-NI-GHAGUTHABENANT   PIC S9(4) COMP.                 
             15 PO-DRKI1272-N-GHKMLSZHLNG   PIC S9(4) COMP.                     
             15 PO-DRKI1272-N-GHKMLNZHLNG   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
           05  PO-DRKI1272-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI1272-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PO-DRKI1272-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI1272-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PO-DRKI1272-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PO-DRKI1272-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
