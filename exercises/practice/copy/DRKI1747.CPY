      *****************************************************************         
      *    COPY-MEMBER: DRKI1747                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1747                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.10.2021 / 12:07:15 / T13EX78            
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
      *    FÜR DIE FUNKTION DRKI1747                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1747.                                                         
           05  PI-DRKI1747-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1747-KDULAUFENDENUMMER   PIC S9(4) COMP.                 
             10 PI-DRKI1747-VERTRAGSID   PIC  X(0013).                          
END   *                                                                         
                                                                                
           05  PI-DRKI1747-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI1747-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1747-DTAERLEDIGTKZ   PIC  X(0001).                       
             10 PI-DRKI1747-KDUBEARBEITER   PIC  X(0030).                       
             10 PI-DRKI1747-KDUEBERTRAGUNGSART   PIC  X(0001).                  
             10 PI-DRKI1747-KDUEIGENVERMKZ   PIC  X(0001).                      
             10 PI-DRKI1747-KDUGUELTIGABDAT   PIC  X(010).                      
             10 PI-DRKI1747-KDUKKVLID   PIC  X(0013).                           
             10 PI-DRKI1747-KDUWARTEZEITKZ   PIC  X(0001).                      
             10 PI-DRKI1747-KDUWIDERSPRUCH   PIC  X(0001).                      
             10 PI-DRKI1747-VBALTID   PIC  X(0013).                             
             10 PI-DRKI1747-VBALTROLLENKZ   PIC  X(0001).                       
             10 PI-DRKI1747-VBNEUID   PIC  X(0013).                             
             10 PI-DRKI1747-VBNEUROLLENKZ   PIC  X(0001).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1747-NI1.                                                     
           05 PI-DRKI1747-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI1747-NI7.                                                     
           05 PI-DRKI1747-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI1747.                                                         
           05  PO-DRKI1747-RC                     PIC  X(003).                  
               88 PO-DRKI1747-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI1747-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
