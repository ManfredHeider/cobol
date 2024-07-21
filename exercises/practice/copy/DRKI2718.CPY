      *****************************************************************         
      *    COPY-MEMBER: DRKI2718                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2718                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.09.2022 / 17:58:13 / U131774            
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
      *    FÜR DIE FUNKTION DRKI2718                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2718.                                                         
           05  PI-DRKI2718-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2718-GHAFONDID   PIC S9(4) COMP.                         
             10 PI-DRKI2718-GHAGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI2718-GHALFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI2718-GHAVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI2718-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI2718-NI-GHAWPBEZ-KNR     PIC S9(4) COMP.                 
             15 PI-DRKI2718-NI-GHAWPBEZ-KTY     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2718-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2718-GHAANLEGERIDOIF   PIC  X(0014).                     
             10 PI-DRKI2718-GHAANTEILEFREI   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2718-GHAANTEILEVORJ   PIC S9(6)V9(4) COMP-3.             
             10 PI-DRKI2718-GHAANTEILGEKOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAANTEILGESPOIF   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2718-GHAANTEILVEFOIF   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAANTERWVORJU13   PIC S9(7)V9(4) COMP-3.           
             10 PI-DRKI2718-GHAANTGESPERRT1   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAANTGESPERRT2   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAANTGESPERRT3   PIC S9(7)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAEINSTANDSKURS   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2718-GHAFLAGEINSTKURS   PIC  X(0001).                    
             10 PI-DRKI2718-GHAGUTHABEN   PIC S9(9)V9(2) COMP-3.                
             10 PI-DRKI2718-GHAGUTHABENANT   PIC S9(7)V9(4) COMP-3.             
             10 PI-DRKI2718-GHAGUTHABENVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2718-GHAGUTHABENWKZ   PIC  X(0001).                      
             10 PI-DRKI2718-GHAHISAUSZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2718-GHAHISEINZAHLUNG   PIC S9(11)V9(2) COMP-3.          
             10 PI-DRKI2718-GHAKAPITALAKT   PIC S9(9)V9(2) COMP-3.              
             10 PI-DRKI2718-GHAKAPITALVORJ   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2718-GHAKUMAUSZHALBJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2718-GHAKUMAUTOWA   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI2718-GHAKUMBARAUSBR   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2718-GHAKUMBARAUSBRVJ   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2718-GHAKUMUEBERTRGES   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2718-GHAKUMUEBERTRVOR   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI2718-GHAKUMULAUSZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2718-GHAKUMULEINZAHLUNG   PIC S9(13)V COMP-3.            
             10 PI-DRKI2718-GHALIEFERDATUM   PIC  X(010).                       
             10 PI-DRKI2718-GHANRGWINKLBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2718-GHANRGWINKLBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2718-GHANRGWOHNEBBA   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI2718-GHANRGWOHNEBBAVH   PIC S9(6)V9(2) COMP-3.           
             10 PI-DRKI2718-GHAREALGEWVERL   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2718-GHARUECKBRUGES   PIC S9(9)V9(2) COMP-3.             
             10 PI-DRKI2718-GHARUECKBRUVORJ   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI2718-GHARUECKNAHMEPR   PIC S9(5)V9(4) COMP-3.            
             10 PI-DRKI2718-GHAVLSPERRDAT1   PIC  X(010).                       
             10 PI-DRKI2718-GHAVLSPERRDAT2   PIC  X(010).                       
             10 PI-DRKI2718-GHAVLSPERRDAT3   PIC  X(010).                       
             10 PI-DRKI2718-GHAWPBEZ-KNR   PIC S9(15)V COMP-3.                  
             10 PI-DRKI2718-GHAWPBEZ-KTY   PIC S9(4) COMP.                      
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2718-NI1.                                                     
           05 PI-DRKI2718-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 2.                                 
       01  PI-DRKI2718-NI7.                                                     
           05 PI-DRKI2718-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 2.                                 
                                                                                
      *                                                                         
       01  PO-DRKI2718.                                                         
           05  PO-DRKI2718-RC                     PIC  X(003).                  
               88 PO-DRKI2718-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2718-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
