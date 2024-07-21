      *****************************************************************         
      *    COPY-MEMBER: DRKI1247                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1247                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:57:53 / U131774            
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
      *    FÜR DIE FUNKTION DRKI1247                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1247.                                                         
           05  PI-DRKI1247-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1247-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI1247-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI1247-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI1247-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI1247-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI1247-NI-GHAGUTHABEN      PIC S9(4) COMP.                 
             15 PI-DRKI1247-NI-GHAGUTHABENANT   PIC S9(4) COMP.                 
             15 PI-DRKI1247-N-GHKMLSZHLNG       PIC S9(4) COMP.                 
             15 PI-DRKI1247-N-GHKMLNZHLNG       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI1247-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1247-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI1247-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI1247-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI1247-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI1247-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1247-NI1.                                                     
           05 PI-DRKI1247-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 4.                                     
       01  PI-DRKI1247-NI7.                                                     
           05 PI-DRKI1247-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 4.                                     
      *                                                                         
       01  PO-DRKI1247.                                                         
           05  PO-DRKI1247-RC                     PIC  X(003).                  
               88 PO-DRKI1247-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI1247-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
