      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14.01.2020                                     
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2021 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   DRKI3222.CPY  $                                         
      *    $Revision:   2.5  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/DRKI3222.CPv  $                                    
      *                                                                         
      *   Rev 2.5   Dec 11 2020 13:13:06   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.4   Dec 02 2020 15:21:18   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Mar 06 2020 17:09:36   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:54:15 U130176                                
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
      *****************************************************************         
      *    COPY-MEMBER: DRKI3222                                      *         
      *    --------------------------------------------------------   *         
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRKI3222                    *         
      *    --------------------------------------------------------   *         
      ****************************************************************          
      *   ERZEUGT MIT OZPGEN         17.06.2021 / 13:38:22 / T13AC20            
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
      *    FÜR DIE FUNKTION DRKI3222                                            
      *    AUSSER: DEM FUNKTIONSNAMEN,                                          
      *            DEM ERR-ERROR-BEREICH UND                                    
      *            DEM STANDARD-STATUS-BEREICH                                  
      *            (JEWEILS SEPARATE COPYS)                                     
      *                                                                         
      *    >>>  UPDATE  <<<                                                     
      *                                                                         
      ****************************************************************          
      *                                                                         
       01  PI-DRKI3222.                                                         
           05  PI-DRKI3222-KEY.                                                 
                                                                                
                                                                                
ANF   *        %M_PI_KEY%                                                       
             10 PI-DRKI3222-LPRVERTRAGSID   PIC  X(0013).                       
END   *                                                                         
                                                                                
           05  PI-DRKI3222-NI.                                                  
                                                                                
ANF   *        %M_PI_DATEN_NI%                                                  
             15 PI-DRKI3222-NI-LPRBEITRAG-VUE   PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRBERECH-KZN    PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRGUTHABEN-VUE  PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRJAHRBTG       PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRSUMDK         PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRSUMME-VUE     PIC S9(4) COMP.                 
             15 PI-DRKI3222-NI-LPRZAHLBTG       PIC S9(4) COMP.                 
END   *                                                                         
                                                                                
           05  PI-DRKI3222-DATEN.                                               
                                                                                
ANF   *        %M_PI_DATEN%                                                     
             10 PI-DRKI3222-LPRBEITRAG-VUE   PIC S9(11)V9(2) COMP-3.            
             10 PI-DRKI3222-LPRBERECH-KZN   PIC  X(0001).                       
             10 PI-DRKI3222-LPRGUTHABEN-VUE   PIC S9(11)V9(2) COMP-3.           
             10 PI-DRKI3222-LPRJAHRBTG   PIC S9(11)V9(2) COMP-3.                
             10 PI-DRKI3222-LPRSUMDK   PIC S9(11)V9(2) COMP-3.                  
             10 PI-DRKI3222-LPRSUMME-VUE   PIC S9(11)V9(2) COMP-3.              
             10 PI-DRKI3222-LPRZAHLBTG   PIC S9(11)V9(2) COMP-3.                
END   *                                                                         
                                                                                
      *  INITIALISIERUNG NULL-INDIKATOR                                         
       01  PI-DRKI3222-NI1.                                                     
           05 PI-DRKI3222-INIT-TAB1 PIC S9(04) COMP VALUE -1                    
                                  OCCURS 7.                                     
       01  PI-DRKI3222-NI7.                                                     
           05 PI-DRKI3222-INIT-TAB7 PIC S9(04) COMP VALUE -7                    
                                  OCCURS 7.                                     
      *                                                                         
       01  PO-DRKI3222.                                                         
           05  PO-DRKI3222-RC                     PIC  X(003).                  
               88 PO-DRKI3222-RC-OK               VALUE 'OK '.                  
               88 PO-DRKI3222-RC-NOT-FOUND        VALUE 'NFD'.                  
      *                                                                         
      ******* ENDE COPY-MEMBER ***************************************          
