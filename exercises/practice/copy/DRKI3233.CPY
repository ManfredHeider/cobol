      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3233.CPY  $                                         
      *    $Revision:   2.5  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3233.CPv  $                                    
      *                                                                         
      *   Rev 2.5   Dec 11 2020 13:13:52   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.4   Dec 02 2020 15:22:04   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:09:54   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 18:04:18 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3233                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3233                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.06.2021 / 13:37:47 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3233                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3233.                                                         
           05  PI-DRKI3233-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3233-LPTLFDNR   PIC S9(4) COMP.                          
             10 PI-DRKI3233-LPTVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3233.                                                         
           05  PO-DRKI3233-RC                     PIC  X(003).                  
               88 PO-DRKI3233-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3233-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3233-NI.                                                  
               10 NI-DRKI3233-TAB.                                              
                  15 DRKI3233-ELE                                               
                     PIC S9(04) COMP OCCURS 7 TIMES.                            
               10 NI-DRKI3233 REDEFINES NI-DRKI3233-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3233-NI-LPTABLAUF   PIC S9(4) COMP.                      
             15 PO-DRKI3233-NI-LPTABLBZ   PIC S9(4) COMP.                       
             15 PO-DRKI3233-NI-LPTBEGINN   PIC S9(4) COMP.                      
             15 PO-DRKI3233-NI-LPTRTBTGVT   PIC S9(4) COMP.                     
             15 PO-DRKI3233-NI-LPTTARIFBEITRAG   PIC S9(4) COMP.                
             15 PO-DRKI3233-NI-LPTVZUST-D-DAT   PIC S9(4) COMP.                 
             15 PO-DRKI3233-NI-LPTZAHLBTGVT   PIC S9(4) COMP.                   
END   *                                                                         
                                                                                
           05  PO-DRKI3233-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3233-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3233-LPTABLAUF   PIC  X(010).                            
             10 PO-DRKI3233-LPTABLBZ   PIC  X(010).                             
             10 PO-DRKI3233-LPTBEGINN   PIC  X(010).                            
             10 PO-DRKI3233-LPTBEITRZUST-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3233-LPTBEITRZUST-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3233-LPTBEZUGLFDNR   PIC S9(4) COMP.                     
             10 PO-DRKI3233-LPTHVT-KZN   PIC  X(0001).                          
             10 PO-DRKI3233-LPTRTBTGVT   PIC S9(11)V9(2) COMP-3.                
             10 PO-DRKI3233-LPTTARIFBEITRAG   PIC S9(11)V9(2) COMP-3.           
             10 PO-DRKI3233-LPTTARKLASSE-KNR   PIC S9(15)V COMP-3.              
             10 PO-DRKI3233-LPTTARKLASSE-KTY   PIC S9(4) COMP.                  
             10 PO-DRKI3233-LPTVERSSUMME   PIC S9(11)V9(2) COMP-3.              
             10 PO-DRKI3233-LPTVZUST-D-DAT   PIC  X(010).                       
             10 PO-DRKI3233-LPTVZUST-D-KNR   PIC S9(4) COMP.                    
             10 PO-DRKI3233-LPTVZUST-D-KTY   PIC S9(4) COMP.                    
             10 PO-DRKI3233-LPTZAHLBTGVT   PIC S9(11)V9(2) COMP-3.              
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
