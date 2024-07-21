      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21.09.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2424.CPY  $                                      

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2424.CPv  $                             
      *
      *   Rev 2.1   Sep 29 2010 10:50:44   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 21 2010 14:43:05 T13EX14                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2424                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2424                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         23.09.2010 / 15:20:27 / T13EX14    
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
      *    FšR DIE FUNKTION DRKI2424                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  UPDATE  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2424.                                                 
           05  PI-DRKI2424-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2424-SWIVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2424-SWIVEZLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
                                                                        
           05  PI-DRKI2424-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PI_DATEN%                                             
             10 PI-DRKI2424-SWIBUENDLBEZ-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2424-N-SWBNDLBZ-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWIBUENDLBEZ-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2424-N-SWBNDLBZ-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWIGESAMTBEITRAG   PIC S9(11)V9(2) COMP-3.  
             10 PI-DRKI2424-SWIHOSTID   PIC  X(0015).                   
             10 PI-DRKI2424-SWIINVSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2424-SWIKFZKZ   PIC  X(0012).                    
             10 PI-DRKI2424-SWIRATENBEITRAG   PIC S9(11)V9(2) COMP-3.   
             10 PI-DRKI2424-SWIRENTENART-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2424-N-SWRNTNRT-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWIRENTENART-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2424-N-SWRNTNRT-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWITAGEGELD   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2424-SWITAGEGELDTT   PIC S9(4) COMP.             
             10 PI-DRKI2424-SWIUNFALLTODPR   PIC S9(11)V9(2) COMP-3.    
             10 PI-DRKI2424-SWIUNFKAPINVPROZ   PIC S9(3)V COMP-3.       
             10 PI-DRKI2424-SWIUNFKAPVAR-KNR   PIC S9(15)V COMP-3.      
             10 PI-DRKI2424-N-SWNFKPVR-KNR     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWIUNFKAPVAR-KTY   PIC S9(4) COMP.          
             10 PI-DRKI2424-N-SWNFKPVR-KTY     PIC S9(4) COMP.          
             10 PI-DRKI2424-SWIUNFSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PI-DRKI2424-SWIVERSSUMME   PIC S9(11)V9(2) COMP-3.      
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
       01  PO-DRKI2424.                                                 
           05  PO-DRKI2424-RC                     PIC  X(003).          
               88 PO-DRKI2424-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2424-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
