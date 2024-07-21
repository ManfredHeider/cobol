      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3220.CPY  $                                         
      *    $Revision:   2.5  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3220.CPv  $                                    
      *                                                                         
      *   Rev 2.5   Dec 11 2020 13:11:32   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.4   Dec 02 2020 15:19:52   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:09:28   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:54:11 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3220                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3220                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.06.2021 / 13:38:19 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3220                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  SELECT  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3220.                                                         
           05  PI-DRKI3220-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3220-LPRVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
      *                                                                         
       01  PO-DRKI3220.                                                         
           05  PO-DRKI3220-RC                     PIC  X(003).                  
               88 PO-DRKI3220-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3220-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
           05  PO-DRKI3220-NI.                                                  
               10 NI-DRKI3220-TAB.                                              
                  15 DRKI3220-ELE                                               
                     PIC S9(04) COMP OCCURS 6 TIMES.                            
               10 NI-DRKI3220 REDEFINES NI-DRKI3220-TAB.                        
                                                                                
ANF   *        %M_PO_DATEN_NI%                                                  
             15 PO-DRKI3220-NI-LPRBEITRAG-VUE   PIC S9(4) COMP.                 
             15 PO-DRKI3220-NI-LPRGUTHABEN-VUE   PIC S9(4) COMP.                
             15 PO-DRKI3220-NI-LPRJAHRBTG   PIC S9(4) COMP.                     
             15 PO-DRKI3220-NI-LPRSUMDK   PIC S9(4) COMP.                       
             15 PO-DRKI3220-NI-LPRSUMME-VUE   PIC S9(4) COMP.                   
             15 PO-DRKI3220-NI-LPRZAHLBTG   PIC S9(4) COMP.                     
END   *                                                                         
                                                                                
           05  PO-DRKI3220-DATEN.                                               
                                                                                
                                                                                
ANF   *        %M_PO_DATEN%                                                     
             10 PO-DRKI3220-LETZTEAENDERUNG   PIC  X(0026).                     
             10 PO-DRKI3220-LPRBEITRAG-VUE   PIC S9(11)V9(2) COMP-3.            
             10 PO-DRKI3220-LPRBERECH-KZN   PIC  X(0001).                       
             10 PO-DRKI3220-LPRGUTHABEN-VUE   PIC S9(11)V9(2) COMP-3.           
             10 PO-DRKI3220-LPRJAHRBTG   PIC S9(11)V9(2) COMP-3.                
             10 PO-DRKI3220-LPRSUMDK   PIC S9(11)V9(2) COMP-3.                  
             10 PO-DRKI3220-LPRSUMME-VUE   PIC S9(11)V9(2) COMP-3.              
             10 PO-DRKI3220-LPRZAHLBTG   PIC S9(11)V9(2) COMP-3.                
END   *                                                                         
                                                                                
                                                                                
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
