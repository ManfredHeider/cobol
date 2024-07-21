      *****************************************************************         
      *    COPY-MEMBER: DRKI2594                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2594                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.10.2021 / 12:07:22 / T13EX78            
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
      *    FÜR DIE FUNKTION DRKI2594                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI2594.                                                         
           05  PI-DRKI2594-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI2594-KDULAUFENDENUMMER   PIC S9(4) COMP.                 
             10 PI-DRKI2594-VERTRAGSID   PIC  X(0013).                          
END   *                                                                         
                                                                                
           05  PI-DRKI2594-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI2594-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI2594-DTAERLEDIGTKZ   PIC  X(0001).                       
             10 PI-DRKI2594-KDUBEARBEITER   PIC  X(0030).                       
             10 PI-DRKI2594-KDUEBERTRAGUNGSART   PIC  X(0001).                  
             10 PI-DRKI2594-KDUEIGENVERMKZ   PIC  X(0001).                      
             10 PI-DRKI2594-KDUGUELTIGABDAT   PIC  X(010).                      
             10 PI-DRKI2594-KDUKKVLID   PIC  X(0013).                           
             10 PI-DRKI2594-KDUTICKETID   PIC S9(18)V COMP-3.                   
             10 PI-DRKI2594-KDUWARTEZEITKZ   PIC  X(0001).                      
             10 PI-DRKI2594-KDUWIDERSPRUCH   PIC  X(0001).                      
             10 PI-DRKI2594-VBALTID   PIC  X(0013).                             
             10 PI-DRKI2594-VBALTROLLENKZ   PIC  X(0001).                       
             10 PI-DRKI2594-VBNEUID   PIC  X(0013).                             
             10 PI-DRKI2594-VBNEUROLLENKZ   PIC  X(0001).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI2594-NI1.                                                     
           05 PI-DRKI2594-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI2594-NI7.                                                     
           05 PI-DRKI2594-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI2594.                                                         
           05  PO-DRKI2594-RC                     PIC  X(003).                  
               88 PO-DRKI2594-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI2594-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
