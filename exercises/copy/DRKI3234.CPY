      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3234.CPY  $                                         
      *    $Revision:   2.6  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3234.CPv  $                                    
      *                                                                         
      *   Rev 2.6   Dec 11 2020 13:14:38   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.5   Dec 02 2020 15:22:52   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.4   Mar 06 2020 17:10:00   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 18:04:20 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3234                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3234                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.06.2021 / 13:37:49 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3234                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  INSERT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3234.                                                         
           05  PI-DRKI3234-KEY.                                                 
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3234-LPTLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3234-LPTVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3234-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3234-NI-LPTABLAUF        PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTABLBZ         PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTBEGINN        PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTRTBTGVT       PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTTARIFBEITRAG  PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTVZUST-D-DAT   PIC S9(4) COMP.                 
             15 PI-DRKI3234-NI-LPTZAHLBTGVT     PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3234-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3234-LPTABLAUF   PIC  X(010).                            
             10 PI-DRKI3234-LPTABLBZ   PIC  X(010).                             
             10 PI-DRKI3234-LPTBEGINN   PIC  X(010).                            
             10 PI-DRKI3234-LPTBEITRZUST-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3234-LPTBEZUGLFDNR   PIC S9(4) COMP.                     
             10 PI-DRKI3234-LPTHVT-KZN   PIC  X(0001).                          
             10 PI-DRKI3234-LPTRTBTGVT   PIC S9(11)V9(2) COMP-3.                
             10 PI-DRKI3234-LPTTARIFBEITRAG   PIC S9(11)V9(2) COMP-3.           
             10 PI-DRKI3234-LPTTARKLASSE-KNR   PIC S9(15)V COMP-3.              
             10 PI-DRKI3234-LPTVERSSUMME   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI3234-LPTVZUST-D-DAT   PIC  X(010).                       
             10 PI-DRKI3234-LPTVZUST-D-KNR   PIC S9(4) COMP.                    
             10 PI-DRKI3234-LPTZAHLBTGVT   PIC S9(11)V9(2) COMP-3.              
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3234-NI1.                                                     
           05 PI-DRKI3234-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                      OCCURS 7.                                 
       01  PI-DRKI3234-NI7.                                                     
           05 PI-DRKI3234-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                      OCCURS 7.                                 
                                                                                
      *                                                                         
       01  PO-DRKI3234.                                                         
           05  PO-DRKI3234-RC                     PIC  X(003).                  
               88 PO-DRKI3234-RC-OK                  VALUE 'OK '.               
               88 PO-DRKI3234-RC-DUPLICATE-VALUE VALUE 'DVA'.                   
      *                                                                         
      ******* ENDE COPY-MEMBER **************************************           
