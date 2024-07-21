      *****************************************************************         
      *    COPY-MEMBER: DRKI2513                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2513                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         25.07.2022 / 14:13:04 / U131848            
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
      *    FÜR DIE FUNKTION DRKI2513                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2513.                                                         
           05  PI-DRKI2513-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2513-LFOVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI2513.                                                         
           05  PO-DRKI2513-RC                     PIC  X(003).                  
               88 PO-DRKI2513-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2513-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI2513-DATEN.                                               
             10   PO-DRKI2513-COUNTER           PIC S9(8) COMP.                 
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
