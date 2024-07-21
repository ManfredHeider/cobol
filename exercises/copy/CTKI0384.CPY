      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CTKI0384.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/CTKI0384.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Dec 11 2020 13:19:56   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Dec 02 2020 15:28:26   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:46   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 17:54:06   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
CCC005*    EXEC SQL DECLARE LBNPSRECHENWERT TABLE                               
CCC004*    ( LPRVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LPRBERECH_KZN                  CHAR(1) NOT NULL,                   
CCC004*      LPRZAHLBTG                     DECIMAL(13, 2),                     
CCC004*      LPRSUMDK                       DECIMAL(13, 2),                     
CCC004*      LPRBEITRAG_VUE                 DECIMAL(13, 2),                     
CCC004*      LPRSUMME_VUE                   DECIMAL(13, 2),                     
CCC004*      LPRGUTHABEN_VUE                DECIMAL(13, 2),                     
CCC004*      LPRJAHRBTG                     DECIMAL(13, 2)                      
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE LBNPSRECHENWERT             *               
      ******************************************************************        
       01  LBNPSRECHENWERT-GESAMT.                                              
        05 NI-LBNPSRECHENWERT-TAB.                                              
           10 NI-LBNPSRECHENWERT-ELE PIC S9(04) COMP OCCURS 9 TIMES.            
        05 NI-LBNPSRECHENWERT REDEFINES NI-LBNPSRECHENWERT-TAB.                 
           10 NI-LPRVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LPRBERECH-KZN      PIC S9(4) COMP.                             
           10 NI-LPRZAHLBTG         PIC S9(4) COMP.                             
           10 NI-LPRSUMDK           PIC S9(4) COMP.                             
           10 NI-LPRBEITRAG-VUE     PIC S9(4) COMP.                             
           10 NI-LPRSUMME-VUE       PIC S9(4) COMP.                             
           10 NI-LPRGUTHABEN-VUE    PIC S9(4) COMP.                             
           10 NI-LPRJAHRBTG         PIC S9(4) COMP.                             
        05 DCL-LBNPSRECHENWERT.                                                 
           10 LPRVERTRAGSID        PIC X(13).                                   
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LPRBERECH-KZN        PIC X(1).                                    
           10 LPRZAHLBTG           PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPRSUMDK             PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPRBEITRAG-VUE       PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPRSUMME-VUE         PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPRGUTHABEN-VUE      PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPRJAHRBTG           PIC S9(11)V9(2) USAGE COMP-3.                
      ************************** ENDE-DCLGEN *************************          
