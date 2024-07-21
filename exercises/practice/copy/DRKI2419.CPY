      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  20.09.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2419.CPY  $                                      

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2419.CPv  $                             
      *
      *   Rev 2.1   Sep 29 2010 10:50:10   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 20 2010 13:39:44 T13EX27                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2419                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2419                    *
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         20.09.2010 / 13:39:44 / T13EX27    
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
      *    FÜR DIE FUNKTION DRKI2419
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2419.                                                 
           05  PI-DRKI2419-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2419-LANDINTERNATKZ   PIC  X(0003).              
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2419.                                                 
           05  PO-DRKI2419-RC                     PIC  X(003).          
               88 PO-DRKI2419-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2419-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI2419-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI2419-LANDISOKZ   PIC  X(0002).                   
             10 PO-DRKI2419-LANDTEXT   PIC  X(0030).                    
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
