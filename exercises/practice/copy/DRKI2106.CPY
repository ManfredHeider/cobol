      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  27.09.2007                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2106.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2106.CPv  $                            
      *
      *   Rev 2.2   Jun 24 2010 14:37:36   t13ex51
      *AKI00974: TEXT-SCHL. für Tab. Einzelvertrag
      *                                                                 
      *   Rev 2.1   17 Oct 2007 11:17:28   t13ex27                      
      *AKL00665: Allfinanz; Firmenkunden AML-Standard-OZPs              
      *                                                                 
      *   Rev 2.0   27 Sep 2007 14:23:30   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2106                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI2106                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         14.06.2010 / 14:04:31 / T13EX27    
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
      *    FÜR DIE FUNKTION DRKI2106                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  DELETE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2106.                                                 
           05  PI-DRKI2106-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2106-VEZVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2106-VEZVETLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2106.                                                 
           05  PO-DRKI2106-RC                     PIC  X(003).          
               88 PO-DRKI2106-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2106-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
