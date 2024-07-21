      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  16.08.2021                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3896.CPY     $                                      
      *    $Revision:   2.0  $                                                  
      *                                                                         
      *    $Log:   L:/mf/DRKI3896.CPv     $                                     
      *                                                                         
      *   Rev 2.0   Aug 16 2021 15:39:28 T13EX39                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3896                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3896                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         16.08.2021 / 15:39:28 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3896                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3896.                                                         
           05  PI-DRKI3896-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3896-FOZLFDNR     PIC S9(4) COMP.                        
             10 PI-DRKI3896-FOZGUTHABPRODLFDNR   PIC S9(4) COMP.                
             10 PI-DRKI3896-FOZVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3896.                                                         
           05  PO-DRKI3896-RC                     PIC  X(003).                  
               88 PO-DRKI3896-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3896-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3896-DATEN.                                               
             10   PO-DRKI3896-COUNTER           PIC S9(8) COMP.                 
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
