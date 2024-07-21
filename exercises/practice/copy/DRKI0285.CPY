      *****************************************************************         
      *    COPY-MEMBER: DRKI0285                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0285                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:49 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0285                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0285.                                                         
           05  PI-DRKI0285-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0285-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0285-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0285-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI0285-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0285.                                                         
           05  PO-DRKI0285-RC                     PIC  X(003).                  
               88 PO-DRKI0285-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0285-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
