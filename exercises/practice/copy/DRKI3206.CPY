      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  16.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3206.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3206.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Jan 19 2021 08:51:18   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.a - allgemeine Vertragsdaten             
      * (LBNPSVERTRAG) erweitern: Sourcen Db2                                   
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:08:54   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 16 2020 14:20:39 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3206                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3206                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         16.03.2021 / 13:07:46 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3206                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3206.                                                         
           05  PI-DRKI3206-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3206-G20VVNR   PIC  X(0015).                             
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3206.                                                         
           05  PO-DRKI3206-RC                     PIC  X(003).                  
               88 PO-DRKI3206-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3206-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
ANF   *        %M_PO_DATEN_NI%                                                  
END   *                                                                         
                                                                                
           05  PO-DRKI3206-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3206-G20VBAV-KZN   PIC  X(0001).                         
             10 PO-DRKI3206-G20VGSMTTAR-BEZ   PIC  X(0025).                     
             10 PO-DRKI3206-G20VINFSPERR-KZN   PIC  X(0001).                    
             10 PO-DRKI3206-G20VMAKLER-KZN   PIC  X(0001).                      
             10 PO-DRKI3206-G20VMANDANT   PIC S9(9) COMP.                       
             10 PO-DRKI3206-G20VPRODKLASSE   PIC S9(15)V COMP-3.                
             10 PO-DRKI3206-G20VRNTBEZUG-KZN   PIC  X(0001).                    
             10 PO-DRKI3206-G20VSTORNODAT   PIC  X(0010).                       
             10 PO-DRKI3206-G20VVBNR   PIC  X(0007).                            
             10 PO-DRKI3206-G20VVERARB-KZN   PIC  X(0001).                      
             10 PO-DRKI3206-G20VVERTRIEBSTAT   PIC  X(0001).                    
             10 PO-DRKI3206-G20VVZUST-D   PIC S9(4) COMP.                       
             10 PO-DRKI3206-G20VVZUST-G   PIC S9(4) COMP.                       
             10 PO-DRKI3206-G20VWERBEBEZEICH   PIC S9(15)V COMP-3.              
             10 PO-DRKI3206-G20VZAHLWID   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
