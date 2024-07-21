      *****************************************************************         
      *    COPY-MEMBER: DRKI0664                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0664                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.06.2022 / 14:07:03 / U131848            
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
      *    FÜR DIE FUNKTION DRKI0664                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0664.                                                         
           05  PI-DRKI0664-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0664-KRTTARIFART-KNR   PIC S9(4) COMP.                   
             10 PI-DRKI0664-KRTTARIFNAMEN   PIC  X(0008).                       
             10 PI-DRKI0664-KRTVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0664-KRTVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI0664.                                                         
           05  PO-DRKI0664-RC                     PIC  X(003).                  
               88 PO-DRKI0664-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI0664-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI0664-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI0664-KRT-ANZ-TAR-DUP   PIC S9(4) COMP.                   
             10 PO-DRKI0664-KRTANWARTSCHAFT   PIC  X(0002).                     
             10 PO-DRKI0664-KRTBE-REDUK-BTG   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI0664-KRTDATABGANGVERTR   PIC  X(010).                    
             10 PO-DRKI0664-KRTDATZUGANGVERTR   PIC  X(010).                    
             10 PO-DRKI0664-KRTMONATSBEITRAG   PIC S9(9)V9(2) COMP-3.           
             10 PO-DRKI0664-KRTMONBEITRWKZ   PIC  X(0001).                      
             10 PO-DRKI0664-KRTRISIKOZUSCHLAG   PIC  X(0010).                   
             10 PO-DRKI0664-KRTSELBSTBETEILIG   PIC S9(9)V9(2) COMP-3.          
             10 PO-DRKI0664-KRTSELBSTBETWKZ   PIC  X(0001).                     
             10 PO-DRKI0664-KRTTARIFART-KTY   PIC S9(4) COMP.                   
             10 PO-DRKI0664-KRTVERSLEISTUNG   PIC S9(9)V9(2) COMP-3.            
             10 PO-DRKI0664-KRTVERSLSTWKZ   PIC  X(0001).                       
             10 PO-DRKI0664-LETZTEAENDERUNG   PIC  X(0026).                     
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
