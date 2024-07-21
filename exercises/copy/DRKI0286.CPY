      *****************************************************************         
      *    COPY-MEMBER: DRKI0286                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0286                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:51 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0286                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0286.                                                         
           05  PI-DRKI0286-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0286-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0286-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0286-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI0286-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0286.                                                         
           05  PO-DRKI0286-RC                     PIC  X(003).                  
               88 PO-DRKI0286-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0286-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI0286-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0286-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PO-DRKI0286-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI0286-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PO-DRKI0286-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PO-DRKI0286-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PO-DRKI0286-GHALIEFERDATUM   PIC  X(010).                       
             10 PO-DRKI0286-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
