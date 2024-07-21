      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  21.09.2010                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRKI2422.CPY  $                                      

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRKI2422.CPv  $                             
      *
      *   Rev 2.1   Sep 29 2010 10:50:30   t13ex14
      *AKI00966: DVAG Österreich: DB-Objekte
      *                                                                 
      *   Rev 2.0   Sep 21 2010 14:42:59 T13EX14                        
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRKI2422                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FšR DIE FUNKTION DRKI2422                    * 
      *    --------------------------------------------------------   * 
      ****************************************************************  
      *   ERZEUGT MIT OZPGEN         23.09.2010 / 15:20:22 / T13EX14    
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
      *    FšR DIE FUNKTION DRKI2422                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT  <<<                                             
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRKI2422.                                                 
           05  PI-DRKI2422-KEY.                                         
                                                                        
                                                                        
ANF   *        %M_PI_KEY%                                               
             10 PI-DRKI2422-SWIVERTRAGSID   PIC  X(0013).               
             10 PI-DRKI2422-SWIVEZLFDNR   PIC S9(4) COMP.               
END   *                                                                 
                                                                        
      *                                                                 
       01  PO-DRKI2422.                                                 
           05  PO-DRKI2422-RC                     PIC  X(003).          
               88 PO-DRKI2422-RC-OK               VALUE 'OK '.          
               88 PO-DRKI2422-RC-NOT-FOUND        VALUE 'NFD'.          
      *                                                                 
           05  PO-DRKI2422-DATEN.                                       
                                                                        
                                                                        
ANF   *        %M_PO_DATEN%                                             
             10 PO-DRKI2422-SWIBUENDLBEZ-KNR   PIC S9(15)V COMP-3.      
             10 PO-DRKI2422-N-SWBNDLBZ-KNR   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWIBUENDLBEZ-KTY   PIC S9(4) COMP.          
             10 PO-DRKI2422-N-SWBNDLBZ-KTY   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWIGESAMTBEITRAG   PIC S9(11)V9(2) COMP-3.  
             10 PO-DRKI2422-SWIHOSTID   PIC  X(0015).                   
             10 PO-DRKI2422-SWIINVSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PO-DRKI2422-SWIKFZKZ   PIC  X(0012).                    
             10 PO-DRKI2422-SWIRATENBEITRAG   PIC S9(11)V9(2) COMP-3.   
             10 PO-DRKI2422-SWIRENTENART-KNR   PIC S9(15)V COMP-3.      
             10 PO-DRKI2422-N-SWRNTNRT-KNR   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWIRENTENART-KTY   PIC S9(4) COMP.          
             10 PO-DRKI2422-N-SWRNTNRT-KTY   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWITAGEGELD   PIC S9(11)V9(2) COMP-3.       
             10 PO-DRKI2422-SWITAGEGELDTT   PIC S9(4) COMP.             
             10 PO-DRKI2422-SWIUNFALLTODPR   PIC S9(11)V9(2) COMP-3.    
             10 PO-DRKI2422-SWIUNFKAPINVPROZ   PIC S9(3)V COMP-3.       
             10 PO-DRKI2422-SWIUNFKAPVAR-KNR   PIC S9(15)V COMP-3.      
             10 PO-DRKI2422-N-SWNFKPVR-KNR   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWIUNFKAPVAR-KTY   PIC S9(4) COMP.          
             10 PO-DRKI2422-N-SWNFKPVR-KTY   PIC S9(4) COMP.            
             10 PO-DRKI2422-SWIUNFSUMME   PIC S9(11)V9(2) COMP-3.       
             10 PO-DRKI2422-SWIVERSSUMME   PIC S9(11)V9(2) COMP-3.      
END   *                                                                 
                                                                        
                                                                        
      *                                                                 
      ******* ENDE COPY-MEMBER ***************************************  
