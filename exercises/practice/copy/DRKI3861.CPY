      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  22.12.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3861.CPY  $                                         
      *    $Revision:   2.1  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3861.CPv  $                                    
      *                                                                         
      *   Rev 2.1   Feb 28 2021 10:44:26   t13ex39                              
      *AKI01790: Deutsche Bank - SPARPLÄNE KSDS--->TT                           
      *                                                                         
      *   Rev 2.0   Dec 22 2020 16:03:21 T13EX39                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3861                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3861                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         26.04.2021 / 11:00:16 / T13EX39            
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
      *    FÜR DIE FUNKTION DRKI3861                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  DELETE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3861.                                                         
           05  PI-DRKI3861-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3861-G48SPPLANNR   PIC  X(0002).                         
             10 PI-DRKI3861-G48SPVNR   PIC  X(0015).                            
             10 PI-DRKI3861-G48SPWAEHRUNGSKZ   PIC  X(0003).                    
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3861.                                                         
           05  PO-DRKI3861-RC                     PIC  X(003).                  
               88 PO-DRKI3861-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3861-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
