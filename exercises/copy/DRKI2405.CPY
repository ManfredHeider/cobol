      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  25.05.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2405.CPY  $                                         

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2405.CPv  $                            
      *
      *   Rev 2.2   Sep 29 2010 10:49:50   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.1   Jun 28 2010 10:02:56   t13ex48                      
      *AKI00970: DB-Žnderungen f?r Rel. 5.12: VERTRAG und VERTRAGUR     
      *LBEZ                                                             
      *                                                                 
      *   Rev 2.0   May 25 2010 12:00:57 T13EX27                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2405                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2405                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         10.08.2010 / 10:53:31 / T13EX14    
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
      *    FšR DIE FUNKTION DRKI2405                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2405.                                                 
           05  PI-DRKI2405-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2405-VUBVERTRAGSID   PIC  X(0013).               
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2405-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2405-VUBRDTAZIEL   PIC S9(4) COMP.               
             10 PI-DRKI2405-VUBURL.                                     
                49 PI-DRKI2405-VUBURL-LEN PIC S9(4) COMP.               
                49 PI-DRKI2405-VUBURL-TEXT PIC X(100).                  
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2405.                                                 
           05  PO-DRKI2405-RC                     PIC  X(003).          
               88 PO-DRKI2405-RC-OK                  VALUE 'OK '.       
               88 PO-DRKI2405-RC-DUPLICATE-VALUE VALUE 'DVA'.           
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
