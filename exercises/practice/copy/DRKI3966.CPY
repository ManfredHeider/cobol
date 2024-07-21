      *****************************************************************         
      *    COPY-MEMBER: DRKI3966                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3966                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.09.2022 / 08:45:40 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3966                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3966.                                                         
           05  PI-DRKI3966-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3966-G21VKVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3966.                                                         
           05  PO-DRKI3966-RC                     PIC  X(003).                  
               88 PO-DRKI3966-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3966-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3966-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3966-G21VKANREDE   PIC S9(4) COMP.                       
             10 PO-DRKI3966-G21VKGEBDAT   PIC  X(0010).                         
             10 PO-DRKI3966-G21VKGESCHLECHT   PIC  X(0001).                     
             10 PO-DRKI3966-G21VKLKZN   PIC  X(0003).                           
             10 PO-DRKI3966-G21VKNACHNAME   PIC  X(0080).                       
             10 PO-DRKI3966-G21VKORT   PIC  X(0030).                            
             10 PO-DRKI3966-G21VKPLZ   PIC  X(0015).                            
             10 PO-DRKI3966-G21VKSTR   PIC  X(0045).                            
             10 PO-DRKI3966-G21VKVERARB-KZN   PIC  X(0001).                     
             10 PO-DRKI3966-G21VKVORNAME   PIC  X(0030).                        
             10 PO-DRKI3966-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3966-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
