      *****************************************************************         
      *    COPY-MEMBER: DRKI2443                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2443                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:11 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2443                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2443.                                                         
           05  PI-DRKI2443-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2443-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2443-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2443-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2443-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2443-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2443-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2443-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2443-GHAFLAGEINSTKURS   PIC  X(0001).                    
             10 PI-DRKI2443-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2443-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2443-GHAGUTHABENVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2443-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2443-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2443-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2443-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI2443-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2443-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI2443-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2443-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2443-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2443-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2443-GHALIEFERDATUM   PIC  X(010).                       
             10 PI-DRKI2443-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2443-NI1.                                                     
           05 PI-DRKI2443-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI2443-NI7.                                                     
           05 PI-DRKI2443-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI2443.                                                         
           05  PO-DRKI2443-RC                     PIC  X(003).                  
               88 PO-DRKI2443-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2443-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
