      *****************************************************************         
      *    COPY-MEMBER: DRKI1848                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1848                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:57 / U131774            
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
      *    FÜR DIE FUNKTION DRKI1848                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1848.                                                         
           05  PI-DRKI1848-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1848-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI1848-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI1848-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI1848-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI1848-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI1848-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1848-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI1848-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI1848-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI1848-GHALIEFERDATUM   PIC  X(010).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1848-NI1.                                                     
           05 PI-DRKI1848-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI1848-NI7.                                                     
           05 PI-DRKI1848-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI1848.                                                         
           05  PO-DRKI1848-RC                     PIC  X(003).                  
               88 PO-DRKI1848-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1848-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
