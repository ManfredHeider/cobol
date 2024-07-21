      *****************************************************************         
      *    COPY-MEMBER: DRKI3880                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3880                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 09:55:51 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3880                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3880.                                                         
           05  PI-DRKI3880-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3880-G48-DKKARTENNR   PIC  X(0013).                      
             10 PI-DRKI3880-G48-DKKONTOUNR   PIC  X(0002).                      
             10 PI-DRKI3880-G48-DKVERTRAGSNR   PIC  X(0015).                    
             10 PI-DRKI3880-G48-DKWAEHRUNG   PIC  X(0003).                      
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3880.                                                         
           05  PO-DRKI3880-RC                     PIC  X(003).                  
               88 PO-DRKI3880-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3880-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
