      *****************************************************************         
      *    COPY-MEMBER: DRKI0665                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI0665                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         20.06.2022 / 14:07:09 / U131848            
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
      *    FÜR DIE FUNKTION DRKI0665                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI0665.                                                         
           05  PI-DRKI0665-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI0665-KRTTARIFART-KNR   PIC S9(4) COMP.                   
             10 PI-DRKI0665-KRTTARIFNAMEN   PIC  X(0008).                       
             10 PI-DRKI0665-KRTVERSTARLFDNR   PIC S9(4) COMP.                   
             10 PI-DRKI0665-KRTVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI0665-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI0665-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI0665-KRT-ANZ-TAR-DUP   PIC S9(4) COMP.                   
             10 PI-DRKI0665-KRTANWARTSCHAFT   PIC  X(0002).                     
             10 PI-DRKI0665-KRTBE-REDUK-BTG   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0665-KRTDATABGANGVERTR   PIC  X(010).                    
             10 PI-DRKI0665-KRTDATZUGANGVERTR   PIC  X(010).                    
             10 PI-DRKI0665-KRTMONATSBEITRAG   PIC S9(9)V9(2) COMP-3.           
             10 PI-DRKI0665-KRTMONBEITRWKZ   PIC  X(0001).                      
             10 PI-DRKI0665-KRTRISIKOZUSCHLAG   PIC  X(0010).                   
             10 PI-DRKI0665-KRTSELBSTBETEILIG   PIC S9(9)V9(2) COMP-3.          
             10 PI-DRKI0665-KRTSELBSTBETWKZ   PIC  X(0001).                     
             10 PI-DRKI0665-KRTVERSLEISTUNG   PIC S9(9)V9(2) COMP-3.            
             10 PI-DRKI0665-KRTVERSLSTWKZ   PIC  X(0001).                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI0665-NI1.                                                     
           05 PI-DRKI0665-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 1.                                 
       01  PI-DRKI0665-NI7.                                                     
           05 PI-DRKI0665-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 1.                                 
                                                                                
      *                                                                         
       01  PO-DRKI0665.                                                         
           05  PO-DRKI0665-RC                     PIC  X(003).                  
               88 PO-DRKI0665-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI0665-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
