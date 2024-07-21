      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  16.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3208.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3208.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Jan 19 2021 08:52:50   t13ac20                              
      *AKI01819: MIA: Arbeitspaket 3.1.a - allgemeine Vertragsdaten             
      * (LBNPSVERTRAG) erweitern: Sourcen Db2                                   
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:09:04   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 16 2020 14:20:43 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3208                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3208                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         16.03.2021 / 13:07:52 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3208                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3208.                                                         
           05  PI-DRKI3208-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3208-G20VVNR   PIC  X(0015).                             
END   *                                                                         
                                                                                
           05  PI-DRKI3208-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 FILLER                          PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3208-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3208-G20VBAV-KZN   PIC  X(0001).                         
             10 PI-DRKI3208-G20VGSMTTAR-BEZ   PIC  X(0025).                     
             10 PI-DRKI3208-G20VINFSPERR-KZN   PIC  X(0001).                    
             10 PI-DRKI3208-G20VMAKLER-KZN   PIC  X(0001).                      
             10 PI-DRKI3208-G20VMANDANT   PIC S9(9) COMP.                       
             10 PI-DRKI3208-G20VPRODKLASSE   PIC S9(15)V COMP-3.                
             10 PI-DRKI3208-G20VRNTBEZUG-KZN   PIC  X(0001).                    
             10 PI-DRKI3208-G20VSTORNODAT   PIC  X(0010).                       
             10 PI-DRKI3208-G20VVBNR   PIC  X(0007).                            
             10 PI-DRKI3208-G20VVERARB-KZN   PIC  X(0001).                      
             10 PI-DRKI3208-G20VVERTRIEBSTAT   PIC  X(0001).                    
             10 PI-DRKI3208-G20VVZUST-D   PIC S9(4) COMP.                       
             10 PI-DRKI3208-G20VVZUST-G   PIC S9(4) COMP.                       
             10 PI-DRKI3208-G20VWERBEBEZEICH   PIC S9(15)V COMP-3.              
             10 PI-DRKI3208-G20VZAHLWID   PIC S9(4) COMP.                       
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3208-NI1.                                                     
           05 PI-DRKI3208-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 1.                                     
       01  PI-DRKI3208-NI7.                                                     
           05 PI-DRKI3208-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 1.                                     
      *                                                                         
       01  PO-DRKI3208.                                                         
           05  PO-DRKI3208-RC                     PIC  X(003).                  
               88 PO-DRKI3208-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3208-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
