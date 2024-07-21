      *****************************************************************         
      *    COPY-MEMBER: DRKI2440                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2440                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:09 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2440                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2440.                                                         
           05  PI-DRKI2440-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2440-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2440-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2440-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2440-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2440-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2440-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2440-GHAANLEGERIDOIF   PIC  X(0014).                     
             10 PI-DRKI2440-GHAANTEILGEKOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2440-GHAANTEILGESPOIF   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2440-GHAANTEILVEFOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2440-GHAANTERWVORJU13   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2440-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2440-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2440-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2440-GHAKUMAUSZHALBJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2440-GHALIEFERDATUM   PIC  X(010).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2440-NI1.                                                     
           05 PI-DRKI2440-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI2440-NI7.                                                     
           05 PI-DRKI2440-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI2440.                                                         
           05  PO-DRKI2440-RC                     PIC  X(003).                  
               88 PO-DRKI2440-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI2440-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
