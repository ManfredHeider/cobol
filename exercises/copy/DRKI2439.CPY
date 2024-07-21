      *****************************************************************         
      *    COPY-MEMBER: DRKI2439                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2439                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:08 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2439                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2439.                                                         
           05  PI-DRKI2439-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2439-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2439-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2439-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2439-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2439-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2439-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2439-GHAANLEGERIDOIF   PIC  X(0014).                     
             10 PI-DRKI2439-GHAANTEILGEKOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2439-GHAANTEILGESPOIF   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2439-GHAANTEILVEFOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2439-GHAANTERWVORJU13   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2439-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2439-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2439-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2439-GHAKUMAUSZHALBJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2439-GHALIEFERDATUM   PIC  X(010).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2439-NI1.                                                     
           05 PI-DRKI2439-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI2439-NI7.                                                     
           05 PI-DRKI2439-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI2439.                                                         
           05  PO-DRKI2439-RC                     PIC  X(003).                  
               88 PO-DRKI2439-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2439-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
