      *****************************************************************         
      *    COPY-MEMBER: DRKI3941                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3941                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.05.2022 / 14:02:43 / T13EX78            
      *****************************************************************         
      *    VERSION                                                              
      ****************************************************************          
      * AENDERUNGEN:                                                 *          
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME     *     
      * -------------------------------------------------------------*          
      *         !        !                                !          *     
      *         !        !                                !          *    
      *                                                              *          
      * ENDE-LOGBUCH                                                 *          
      ****************************************************************          
      *                                                                         
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER                    
      *    FÜR DIE FUNKTION DRKI3941                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3941.                                                         
           05  PI-DRKI3941-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3941-AKFFONDSID   PIC S9(4) COMP.                        
             10 PI-DRKI3941-AKFGESID   PIC S9(4) COMP.                          
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3941.                                                         
           05  PO-DRKI3941-RC                     PIC  X(003).                  
               88 PO-DRKI3941-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3941-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3941-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3941-AKFBEZEICHNUNG   PIC  X(0040).                      
             10 PO-DRKI3941-AKFISIN   PIC  X(0020).                             
             10 PO-DRKI3941-AKFVLKZN   PIC  X(0001).                            
             10 PO-DRKI3941-AKFWKN   PIC  X(0006).                              
             10 PO-DRKI3941-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
