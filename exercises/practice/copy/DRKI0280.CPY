      *****************************************************************         
      *    COPY-MEMBER: DRKI0280                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0280                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         19.09.2022 / 17:03:27 / U131774            
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
      *    FÜR DIE FUNKTION DRKI0280                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0280.                                                         
           05  PI-DRKI0280-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0280-GHBBUCHUNGLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0280-GHBFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI0280-GHBGHALFDNR   PIC S9(4) COMP.                       
             10 PI-DRKI0280-GHBGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI0280-GHBVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0280.                                                         
           05  PO-DRKI0280-RC                     PIC  X(003).                  
               88 PO-DRKI0280-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0280-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI0280-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0280-GHBBETRAGLETZTBUCH   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI0280-GHBBETRLBUWKZ   PIC  X(0001).                       
             10 PO-DRKI0280-GHBBUCHUNGSDATUM   PIC  X(010).                     
             10 PO-DRKI0280-GHBGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PO-DRKI0280-GHBKZFINHLETZTBUID   PIC S9(4) COMP.                
             10 PO-DRKI0280-GHBNETTOBETRAG   PIC S9(9)V9(2) COMP-3.             
             10 PO-DRKI0280-GHBNETTOBETWKZ   PIC  X(0001).                      
             10 PO-DRKI0280-GHBSTEUBETRLETZTBU   PIC S9(9)V9(2) COMP-3.         
             10 PO-DRKI0280-GHBSTLETZTBUWKZ   PIC  X(0001).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
