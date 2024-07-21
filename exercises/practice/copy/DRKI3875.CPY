      *****************************************************************         
      *    COPY-MEMBER: DRKI3875                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3875                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 10:42:38 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3875                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3875.                                                         
           05  PI-DRKI3875-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3875-BDKKARTENNR   PIC  X(0013).                         
             10 PI-DRKI3875-BDKLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3875-BDKVERTRAGSID   PIC  X(0013).                       
             10 PI-DRKI3875-BDKWAEHRUNG   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3875.                                                         
           05  PO-DRKI3875-RC                     PIC  X(003).                  
               88 PO-DRKI3875-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3875-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
