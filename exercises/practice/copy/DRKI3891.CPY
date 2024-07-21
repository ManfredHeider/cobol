      *****************************************************************         
      *    COPY-MEMBER: DRKI3891                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3891                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         15.09.2021 / 11:29:55 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3891                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3891.                                                         
           05  PI-DRKI3891-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3891-G40GKOVNR   PIC  X(0015).                           
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3891.                                                         
           05  PO-DRKI3891-RC                     PIC  X(003).                  
               88 PO-DRKI3891-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3891-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3891-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3891-G40GKOBILANZSCHL   PIC  X(0004).                    
             10 PO-DRKI3891-G40GKODATOEFF   PIC  X(0008).                       
             10 PO-DRKI3891-G40GKODELKZ   PIC  X(0001).                         
             10 PO-DRKI3891-G40GKOEXTLIMIT   PIC S9(12)V9(2) COMP-3.            
             10 PO-DRKI3891-G40GKOHABENZINS   PIC S9(2)V9(4) COMP-3.            
             10 PO-DRKI3891-G40GKOKONDSCHL   PIC  X(0002).                      
             10 PO-DRKI3891-G40GKOKUMHABENAM   PIC S9(12)V9(2) COMP-3.          
             10 PO-DRKI3891-G40GKOKUMHABENVM   PIC S9(12)V9(2) COMP-3.          
             10 PO-DRKI3891-G40GKOKUMSOLLAM   PIC S9(12)V9(2) COMP-3.           
             10 PO-DRKI3891-G40GKOKUMSOLLVM   PIC S9(12)V9(2) COMP-3.           
             10 PO-DRKI3891-G40GKOSALDO   PIC S9(12)V9(2) COMP-3.               
             10 PO-DRKI3891-G40GKOSCHLDAT   PIC  X(0008).                       
             10 PO-DRKI3891-G40GKOSOLLZINS   PIC S9(2)V9(4) COMP-3.             
             10 PO-DRKI3891-G40GKOUEBERZINS   PIC S9(2)V9(4) COMP-3.            
             10 PO-DRKI3891-G40GKOZRAM   PIC  X(0005).                          
             10 PO-DRKI3891-G40GKOZRVM   PIC  X(0005).                          
             10 PO-DRKI3891-G40GKVORZEICHEN   PIC  X(0001).                     
             10 PO-DRKI3891-LETZTERAENDERER   PIC  X(0010).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
