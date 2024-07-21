      *****************************************************************         
      *    COPY-MEMBER: DRKI3888                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3888                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.09.2021 / 11:29:45 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3888                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3888.                                                         
           05  PI-DRKI3888-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3888-G40GKOVNR   PIC  X(0015).                           
END   *                                                                         
                                                                                
           05  PI-DRKI3888-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3888-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3888-G40GKOBILANZSCHL   PIC  X(0004).                    
             10 PI-DRKI3888-G40GKODATOEFF   PIC  X(0008).                       
             10 PI-DRKI3888-G40GKODELKZ   PIC  X(0001).                         
             10 PI-DRKI3888-G40GKOEXTLIMIT   PIC S9(12)V9(2) COMP-3.            
             10 PI-DRKI3888-G40GKOHABENZINS   PIC S9(2)V9(4) COMP-3.            
             10 PI-DRKI3888-G40GKOKONDSCHL   PIC  X(0002).                      
             10 PI-DRKI3888-G40GKOKUMHABENAM   PIC S9(12)V9(2) COMP-3.          
             10 PI-DRKI3888-G40GKOKUMHABENVM   PIC S9(12)V9(2) COMP-3.          
             10 PI-DRKI3888-G40GKOKUMSOLLAM   PIC S9(12)V9(2) COMP-3.           
             10 PI-DRKI3888-G40GKOKUMSOLLVM   PIC S9(12)V9(2) COMP-3.           
             10 PI-DRKI3888-G40GKOSALDO   PIC S9(12)V9(2) COMP-3.               
             10 PI-DRKI3888-G40GKOSCHLDAT   PIC  X(0008).                       
             10 PI-DRKI3888-G40GKOSOLLZINS   PIC S9(2)V9(4) COMP-3.             
             10 PI-DRKI3888-G40GKOUEBERZINS   PIC S9(2)V9(4) COMP-3.            
             10 PI-DRKI3888-G40GKOZRAM   PIC  X(0005).                          
             10 PI-DRKI3888-G40GKOZRVM   PIC  X(0005).                          
             10 PI-DRKI3888-G40GKVORZEICHEN   PIC  X(0001).                     
             10 PI-DRKI3888-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3888-NI1.                                                     
           05 PI-DRKI3888-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI3888-NI7.                                                     
           05 PI-DRKI3888-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3888.                                                         
           05  PO-DRKI3888-RC                     PIC  X(003).                  
               88 PO-DRKI3888-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3888-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
