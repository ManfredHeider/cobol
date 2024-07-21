      *****************************************************************         
      *    COPY-MEMBER: DRKI3967                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3967                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.09.2022 / 08:45:42 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3967                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3967.                                                         
           05  PI-DRKI3967-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3967-G21VKVNR   PIC  X(0015).                            
END   *                                                                         
                                                                                
           05  PI-DRKI3967-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3967-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3967-G21VKANREDE   PIC S9(4) COMP.                       
             10 PI-DRKI3967-G21VKGEBDAT   PIC  X(0010).                         
             10 PI-DRKI3967-G21VKGESCHLECHT   PIC  X(0001).                     
             10 PI-DRKI3967-G21VKLKZN   PIC  X(0003).                           
             10 PI-DRKI3967-G21VKNACHNAME   PIC  X(0080).                       
             10 PI-DRKI3967-G21VKORT   PIC  X(0030).                            
             10 PI-DRKI3967-G21VKPLZ   PIC  X(0015).                            
             10 PI-DRKI3967-G21VKSTR   PIC  X(0045).                            
             10 PI-DRKI3967-G21VKVERARB-KZN   PIC  X(0001).                     
             10 PI-DRKI3967-G21VKVORNAME   PIC  X(0030).                        
             10 PI-DRKI3967-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3967-NI1.                                                     
           05 PI-DRKI3967-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3967-NI7.                                                     
           05 PI-DRKI3967-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3967.                                                         
           05  PO-DRKI3967-RC                     PIC  X(003).                  
               88 PO-DRKI3967-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3967-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
