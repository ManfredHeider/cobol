      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21.09.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2423.CPY  $                                      

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2423.CPv  $                             
      *
      *   Rev 2.1   Sep 29 2010 10:50:38   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 21 2010 14:43:02 T13EX14                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2423                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2423                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         23.09.2010 / 15:20:25 / T13EX14    
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
      *    FšR DIE FUNKTION DRKI2423                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  INSERT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2423.                                                 
           05  PI-DRKI2423-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2423-SWIVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2423-SWIVEZLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2423-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2423-SWIBUENDLBEZ-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2423-N-SWBNDLBZ-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWIBUENDLBEZ-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2423-N-SWBNDLBZ-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWIGESAMTBEITRAG   PIC S9(11)V9(2) COMP-3.  
             10 PI-DRKI2423-SWIHOSTID   PIC  X(0015).                   
             10 PI-DRKI2423-SWIINVSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2423-SWIKFZKZ   PIC  X(0012).                    
             10 PI-DRKI2423-SWIRATENBEITRAG   PIC S9(11)V9(2) COMP-3.   
             10 PI-DRKI2423-SWIRENTENART-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2423-N-SWRNTNRT-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWIRENTENART-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2423-N-SWRNTNRT-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWITAGEGELD   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2423-SWITAGEGELDTT   PIC S9(4) COMP.             
             10 PI-DRKI2423-SWIUNFALLTODPR   PIC S9(11)V9(2) COMP-3.    
             10 PI-DRKI2423-SWIUNFKAPINVPROZ   PIC S9(3)V COMP-3.       
             10 PI-DRKI2423-SWIUNFKAPVAR-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2423-N-SWNFKPVR-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWIUNFKAPVAR-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2423-N-SWNFKPVR-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2423-SWIUNFSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2423-SWIVERSSUMME   PIC S9(11)V9(2) COMP-3.      
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2423.                                                 
           05  PO-DRKI2423-RC                     PIC  X(003).          
               88 PO-DRKI2423-RC-OK                  VALUE 'OK '.       
               88 PO-DRKI2423-RC-DUPLICATE-VALUE VALUE 'DVA'.           
      *                                                                 
      ******* ENDE COPY-MEMBER **************************************   
