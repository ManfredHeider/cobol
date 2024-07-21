      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3226.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3226.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Feb 25 2021 10:53:00   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.abc - DTA's erweitern: Sourc             
      *en Db2                                                                   
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:09:42   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:33:09 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3226                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3226                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.03.2021 / 07:39:18 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3226                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3226.                                                         
           05  PI-DRKI3226-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3226-G20TLFDNR   PIC S9(4) COMP.                         
             10 PI-DRKI3226-G20TVNR   PIC  X(0015).                             
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3226.                                                         
           05  PO-DRKI3226-RC                     PIC  X(003).                  
               88 PO-DRKI3226-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3226-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3226-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3226-G20TABLAUF   PIC  X(0010).                          
             10 PO-DRKI3226-G20TABLBZ   PIC  X(0010).                           
             10 PO-DRKI3226-G20TBEGINN   PIC  X(0010).                          
             10 PO-DRKI3226-G20TBEITRZUSTID   PIC S9(4) COMP.                   
             10 PO-DRKI3226-G20TBEZUGLFDNR   PIC S9(5)V COMP-3.                 
             10 PO-DRKI3226-G20THVT-KZN   PIC  X(0001).                         
             10 PO-DRKI3226-G20TTARIFBEITRAG   PIC S9(11)V9(2) COMP-3.          
             10 PO-DRKI3226-G20TTARIFKLASSE   PIC S9(5)V COMP-3.                
             10 PO-DRKI3226-G20TVERARB-KZN   PIC  X(0001).                      
             10 PO-DRKI3226-G20TVERSSUMME   PIC S9(11)V9(2) COMP-3.             
             10 PO-DRKI3226-G20TVZUST-D   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
