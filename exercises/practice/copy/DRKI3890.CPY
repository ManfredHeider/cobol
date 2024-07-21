      *****************************************************************         
      *    COPY-MEMBER: DRKI3890                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3890                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.09.2021 / 11:29:52 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3890                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3890.                                                         
           05  PI-DRKI3890-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3890-G40GKOVNR   PIC  X(0015).                           
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3890.                                                         
           05  PO-DRKI3890-RC                     PIC  X(003).                  
               88 PO-DRKI3890-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3890-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
