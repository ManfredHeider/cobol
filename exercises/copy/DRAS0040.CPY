      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  05.09.2014                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *    Copyright (c) 2014 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   DRAS0040.CPY  $                                         

      *    $Revision:   2.1  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/DRAS0040.CPv  $                                
      *
      *   Rev 2.1   Oct 27 2014 16:54:12   t13ae36
      *AAS01105: Allfinanz Jahreswettbewerbe 2014-2015Eigen und Gru
      *ppengeschäft
      *                                                                 
      *   Rev 2.0   Sep 05 2014 10:16:00   t13ae36                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ***************************************************************** 
      *    COPY-MEMBER: DRAS0040                                      * 
      *    --------------------------------------------------------   * 
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRAS0040                    * 
      *    --------------------------------------------------------   * 
      *    ERSTELLT: FRANKE                                           * 
      *    DATUM:    05.09.2014                                       * 
      ***************************************************************** 
      * AENDERUNGEN:                                                  * 
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      * 
      * --------+--------+----+---------------------------+-----------* 
      * AAS01105!05.09.14!2.0 ! NEU                       ! FRANKE    * 
      * ENDE-LOGB                                                     * 
      ***************************************************************** 
      *                                                                 
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER            
      *    FÜR DIE FUNKTION DRAS0040                                    
      *    AUSSER: DEM FUNKTIONSNAMEN,                                  
      *            DEM ERR-ERROR-BEREICH UND                            
      *            DEM STANDARD-STATUS-BEREICH                          
      *            (JEWEILS SEPARATE COPYS)                             
      *                                                                 
      *    >>>  SELECT ...                <<<                           
      *                                                                 
      ****************************************************************  
      *                                                                 
       01  PI-DRAS0040.                                                 
           05  PI-DRAS0040-VBNR        PIC  9(07)     VALUE  ZERO.      
           05  PI-DRAS0040-BSE-BETR    PIC  S9(09)V99 COMP VALUE ZERO.  
           05  PI-DRAS0040-Strt-Dat    PIC  x(10)     VALUE Space.      
           05  PI-DRAS0040-Varb-Dat    PIC  X(10)     VALUE Space.      
      *                                                                 
       01  PO-DRAS0040.                                                 
           05  PO-DRAS0040-RC                 PIC  X(003).              
               88 PO-DRAS0040-RC-OK                VALUE 'OK '.         
               88 PO-DRAS0040-RC-NOK               VALUE 'NOK'.         
               88 PO-DRAS0040-RC-NOT-FOUND         VALUE 'NFD'.         
               88 PO-DRAS0040-RC-ERR               VALUE 'ERR'.         
      *                                                                 
           05  PO-DRAS0040-MA-ST-NR           PIC S9(07)       COMP.    
           05  PO-DRAS0040-FAKTOR             PIC  9(03)V9     COMP-3.  
           05  PO-DRAS0040-ANZ-VBA            PIC S9(05)       COMP.    
           05  PO-DRAS0040-ANZ-VBA-P-ERF      PIC S9(05)       COMP.    
           05  PO-DRAS0040-ANZ-AL-P           PIC S9(05)       COMP.    
           05  PO-DRAS0040-ANZ-AL-P-ERF       PIC S9(05)       COMP.    
      *---------------------------------------------------------------* 
      *----   ENDE  COPY-MEMBER  DRAS0040   --------------------------* 
      *---------------------------------------------------------------* 
                                                                        
                                                                        
