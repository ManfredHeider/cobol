      *****************************************************************         
      *    COPY-MEMBER: DRKI0387                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0387                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:29 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0387                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  MIN / MAX <<<                                           
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0387.                                                         
           05  PI-DRKI0387-KEY.                                                 
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0387-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0387-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI0387-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0387-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
      *                                                                         
       01  PO-DRKI0387.                                                         
           05  PO-DRKI0387-RC                     PIC  X(003).                  
               88 PO-DRKI0387-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0387-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI0387-DATEN.                                               
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0387-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
END   *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
