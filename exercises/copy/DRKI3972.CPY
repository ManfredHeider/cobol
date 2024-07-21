      *****************************************************************         
      *    COPY-MEMBER: DRKI3972                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3972                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         06.09.2022 / 14:45:51 / U131774            
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
      *    FÜR DIE FUNKTION DRKI3972                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  COUNT  <<<                                              
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3972.                                                         
           05  PI-DRKI3972-KEY.                                                 
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3972-G79VERTRAGSNR   PIC  X(0015).                       
END   *                                                                         
      *                                                                         
       01  PO-DRKI3972.                                                         
           05  PO-DRKI3972-RC                     PIC  X(003).                  
               88 PO-DRKI3972-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3972-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3972-DATEN.                                               
             10   PO-DRKI3972-COUNTER           PIC S9(4) COMP.                 
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
