      *---------------------------------------------------------------*         
      *    Standard fuer alle Objekte unter PVCS VM                             
      *            Datum      :  14/01/20                                       
      *            Kategorie  :  Copystrecke                                    
      *---------------------------------------------------------------*         
      *                                                                         
      *---------------------------------------------------------------*         
      *    Copyright (c) 2020 ATLAS Dienstleistungen f. V. GmbH                 
      *---------------------------------------------------------------*         
      *    $Workfile:   CTKI0383.CPY  $                                         
      *    $Revision:   2.4  $                                                  
      *                                                                         
      *    $Log:   L:/mf/CPY/CTKI0383.CPv  $                                    
      *                                                                         
      *   Rev 2.4   Dec 11 2020 13:19:10   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.3   Dec 02 2020 15:27:42   t13ac20                              
      *AKI01817: MIA: Arbeitspaket 2 - PSLife-Tabellen anpassen: So             
      *urcen Db2                                                                
      *                                                                         
      *   Rev 2.2   Mar 06 2020 17:10:42   u130176                              
      *AKI01742: Anpassung Schnittstelle PSLife (BU/RFN) Verarbeitung           
      *                                                                         
      *   Rev 2.0   Jan 14 2020 18:03:54   U130176                              
      *Archiverstellung                                                         
      *                                                                         
      *---------------------------------------------------------------*         
CCC005*    EXEC SQL DECLARE LBNPSVERTRAGTEIL TABLE                              
CCC004*    ( LPTVERTRAGSID                  CHAR(13) NOT NULL,                  
CCC004*      LPTLFDNR                       SMALLINT NOT NULL,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,                 
CCC004*      LPTBEZUGLFDNR                  SMALLINT NOT NULL,                  
CCC004*      LPTHVT_KZN                     CHAR(1) NOT NULL,                   
CCC004*      LPTTARKLASSE_KTY               SMALLINT NOT NULL,                  
CCC004*      LPTTARKLASSE_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      LPTBEGINN                      DATE,                               
CCC004*      LPTABLAUF                      DATE,                               
CCC004*      LPTBEITRZUST_KTY               SMALLINT NOT NULL,                  
CCC004*      LPTBEITRZUST_KNR               DECIMAL(15, 0) NOT NULL,            
CCC004*      LPTABLBZ                       DATE,                               
CCC004*      LPTTARIFBEITRAG                DECIMAL(13, 2),                     
CCC004*      LPTRTBTGVT                     DECIMAL(13, 2),                     
CCC004*      LPTZAHLBTGVT                   DECIMAL(13, 2),                     
CCC004*      LPTVERSSUMME                   DECIMAL(13, 2) NOT NULL,            
CCC004*      LPTVZUST_D_KTY                 SMALLINT NOT NULL,                  
CCC004*      LPTVZUST_D_KNR                 SMALLINT NOT NULL,                  
CCC004*      LPTVZUST_D_DAT                 DATE                                
CCC003*    ) END-EXEC.                                                          
      ******************************************************************        
      * COBOL DECLARATION FOR TABLE LBNPSVERTRAGTEIL            *               
      ******************************************************************        
       01  LBNPSVERTRAGTEIL-GESAMT.                                             
        05 NI-LBNPSVERTRAGTEIL-TAB.                                             
           10 NI-LBNPSVERTRAGTEIL-ELE PIC S9(04) COMP OCCURS 19 TIMES.          
        05 NI-LBNPSVERTRAGTEIL REDEFINES NI-LBNPSVERTRAGTEIL-TAB.               
           10 NI-LPTVERTRAGSID      PIC S9(4) COMP.                             
           10 NI-LPTLFDNR           PIC S9(4) COMP.                             
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                             
           10 NI-LPTBEZUGLFDNR      PIC S9(4) COMP.                             
           10 NI-LPTHVT-KZN         PIC S9(4) COMP.                             
           10 NI-LPTTARKLASSE-KTY   PIC S9(4) COMP.                             
           10 NI-LPTTARKLASSE-KNR   PIC S9(4) COMP.                             
           10 NI-LPTBEGINN          PIC S9(4) COMP.                             
           10 NI-LPTABLAUF          PIC S9(4) COMP.                             
           10 NI-LPTBEITRZUST-KTY   PIC S9(4) COMP.                             
           10 NI-LPTBEITRZUST-KNR   PIC S9(4) COMP.                             
           10 NI-LPTABLBZ           PIC S9(4) COMP.                             
           10 NI-LPTTARIFBEITRAG    PIC S9(4) COMP.                             
           10 NI-LPTRTBTGVT         PIC S9(4) COMP.                             
           10 NI-LPTZAHLBTGVT       PIC S9(4) COMP.                             
           10 NI-LPTVERSSUMME       PIC S9(4) COMP.                             
           10 NI-LPTVZUST-D-KTY     PIC S9(4) COMP.                             
           10 NI-LPTVZUST-D-KNR     PIC S9(4) COMP.                             
           10 NI-LPTVZUST-D-DAT     PIC S9(4) COMP.                             
        05 DCL-LBNPSVERTRAGTEIL.                                                
           10 LPTVERTRAGSID        PIC X(13).                                   
           10 LPTLFDNR             PIC S9(4) USAGE COMP.                        
           10 LETZTEAENDERUNG      PIC X(26).                                   
           10 LPTBEZUGLFDNR        PIC S9(4) USAGE COMP.                        
           10 LPTHVT-KZN           PIC X(1).                                    
           10 LPTTARKLASSE-KTY     PIC S9(4) USAGE COMP.                        
           10 LPTTARKLASSE-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 LPTBEGINN            PIC X(10).                                   
           10 LPTABLAUF            PIC X(10).                                   
           10 LPTBEITRZUST-KTY     PIC S9(4) USAGE COMP.                        
           10 LPTBEITRZUST-KNR     PIC S9(15)V USAGE COMP-3.                    
           10 LPTABLBZ             PIC X(10).                                   
           10 LPTTARIFBEITRAG      PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPTRTBTGVT           PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPTZAHLBTGVT         PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPTVERSSUMME         PIC S9(11)V9(2) USAGE COMP-3.                
           10 LPTVZUST-D-KTY       PIC S9(4) USAGE COMP.                        
           10 LPTVZUST-D-KNR       PIC S9(4) USAGE COMP.                        
           10 LPTVZUST-D-DAT       PIC X(10).                                   
      ************************** ENDE-DCLGEN *************************          
