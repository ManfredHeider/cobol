      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  25.05.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2404.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2404.CPv  $                            
      *
      *   Rev 2.2   Sep 29 2010 10:49:38   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.1   Jun 28 2010 10:02:52   t13ex48                      
      *AKI00970: DB-Ùnderungen fÜr Rel. 5.12: VERTRAG und VERTRAGUR     
      *LBEZ                                                             
      *                                                                 
      *   Rev 2.0   May 25 2010 11:16:37 T13EX27                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2404                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2404                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         11.08.2010 / 08:05:28 / T13EX14    
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
      *    DIESES COPY ENTHŽLT DIE EIN- UND AUSGABEPARAMETER            
      *    FšR DIE FUNKTION DRKI2404                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2404.                                                 
           05  PI-DRKI2404-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2404-VUBVERTRAGSID   PIC  X(0013).               
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2404.                                                 
           05  PO-DRKI2404-RC                     PIC  X(003).          
               88 PO-DRKI2404-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2404-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI2404-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI2404-VUBRDTAZIEL   PIC S9(4) COMP.               
             10 PO-DRKI2404-VUBURL.                                     
                49 PO-DRKI2404-VUBURL-LEN PIC S9(4) COMP.               
                49 PO-DRKI2404-VUBURL-TEXT PIC X(100).                  
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
