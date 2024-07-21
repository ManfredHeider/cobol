      *****************************************************************         
      *    COPY-MEMBER: DRKI0274                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0274                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:23 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0274                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0274.                                                         
           05  PI-DRKI0274-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0274-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0274-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0274-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI0274-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0274-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0274.                                                         
           05  PO-DRKI0274-RC                     PIC  X(003).                  
               88 PO-DRKI0274-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0274-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
