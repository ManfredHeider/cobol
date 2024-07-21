      *****************************************************************         
      *    COPY-MEMBER: DRKI3878                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3878                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.10.2022 / 09:55:44 / U130684            
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
      *    FÜR DIE FUNKTION DRKI3878                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3878.                                                         
           05  PI-DRKI3878-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3878-G48-DKKARTENNR   PIC  X(0013).                      
             10 PI-DRKI3878-G48-DKKONTOUNR   PIC  X(0002).                      
             10 PI-DRKI3878-G48-DKVERTRAGSNR   PIC  X(0015).                    
             10 PI-DRKI3878-G48-DKWAEHRUNG   PIC  X(0003).                      
END   *                                                                         
                                                                                
           05  PI-DRKI3878-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3878-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3878-G48-DKDELKZ   PIC  X(0001).                         
             10 PI-DRKI3878-G48-DKGUELTIGBIS   PIC  X(0004).                    
             10 PI-DRKI3878-G48-DKINHNAME1   PIC  X(0027).                      
             10 PI-DRKI3878-G48-DKINHNAME2   PIC  X(0027).                      
             10 PI-DRKI3878-G48-DKKARTENART   PIC  X(0003).                     
             10 PI-DRKI3878-G48-DKNFCFAEHIG   PIC  X(0001).                     
             10 PI-DRKI3878-G48-DKNFCSTATUS   PIC  X(0001).                     
             10 PI-DRKI3878-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3878-NI1.                                                     
           05 PI-DRKI3878-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3878-NI7.                                                     
           05 PI-DRKI3878-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3878.                                                         
           05  PO-DRKI3878-RC                     PIC  X(003).                  
               88 PO-DRKI3878-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3878-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
