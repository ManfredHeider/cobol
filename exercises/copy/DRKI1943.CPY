      *****************************************************************         
      *    COPY-MEMBER: DRKI1943                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI1943                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         23.04.2021 / 10:01:20 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI1943                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  NEXT VALUE<<<                                           
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI1943.                                                         
           05  PI-DRKI1943-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI1943-LPZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI1943-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
                                                                                
           05  PI-DRKI1943-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI1943-LETZTERAENDERER   PIC  X(0010).                     
             10 PI-DRKI1943-LPZBETRAG   PIC S9(11)V9(2) COMP-3.                 
             10 PI-DRKI1943-LPZDATUM   PIC  X(010).                             
             10 PI-DRKI1943-LPZGEVO   PIC  X(0020).                             
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI1943-NI1.                                                     
           05 PI-DRKI1943-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI1943-NI7.                                                     
           05 PI-DRKI1943-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI1943.                                                         
           05  PO-DRKI1943-RC                     PIC  X(003).                  
               88 PO-DRKI1943-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI1943-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
           05  PO-DRKI1943-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI1943-LPZLFDNR   PIC S9(4) COMP.                          
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
