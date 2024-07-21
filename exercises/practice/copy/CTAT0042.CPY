      **--------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  08/09/10                               
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTAT0042.CPY  $                                         




      *    $Revision:   2.5  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTAT0042.CPv  $                            
      *
      *   Rev 2.5   Jan 07 2014 08:48:06   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe ü
      *ber ABLAUFK
      *ONTROLLE - Programme
      *
      *   Rev 2.4   Jan 07 2014 08:26:52   t13ex78
      *AKI01259: Kontrolle tgl. Aktualisierungsläufe über ABLAUFKON
      *TROLLE
      *
      *   Rev 2.3   Sep 09 2010 11:11:06   t13ae49
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen - DTA-Kon
      *. PRG 2 anpassen
      *
      *   Rev 2.2   Sep 09 2010 08:57:22   t13ae49
      *AKI00988: Ablaufkontrolle wegen Preisakt. anpassen - DTA-Kon
      *. PRG 2 anpassen
      *                                                                 
      *   Rev 2.0   Sep 8 2010 09:38:55   T13AE49                       
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
CCC005*    EXEC SQL DECLARE TBAT0042 TABLE                              
CCC004*    ( ABKMANDANTNR                   INTEGER NOT NULL,           
CCC004*      ABKPARTGESID                   SMALLINT NOT NULL,          
CCC004*      ABKJOBNAME                     CHAR(8) NOT NULL,           
CCC004*      ABKLFDNR                       INTEGER NOT NULL,           
CCC004*      ABKPARTNERDATUM                DATE NOT NULL,              
CCC004*      ABKKATALOGDATUM                DATE NOT NULL,              
CCC004*      ABKBEGABLAUF                   TIMESTAMP,                  
CCC004*      ABKBEGAKTU                     TIMESTAMP,                  
CCC004*      ABKENDAKTU                     TIMESTAMP,                  
CCC004*      ABKBEGEINARB                   TIMESTAMP,                  
CCC004*      ABKENDEINARB                   TIMESTAMP,                  
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL,         
CCC004*      ABKABGLSTRSATZKZ               CHAR(1) NOT NULL,
CCC004*      ABKERSTELLTTST                 TIMESTAMP NOT NULL
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE TBAT0042                    *       
      ******************************************************************
       01  TBAT0042-GESAMT.                                             
        05 NI-TBAT0042-TAB.                                             
           10 NI-TBAT0042-ELE PIC S9(04) COMP OCCURS 14 TIMES.
        05 NI-TBAT0042 REDEFINES NI-TBAT0042-TAB.                       
           10 NI-ABKMANDANTNR       PIC S9(4) COMP.                     
           10 NI-ABKPARTGESID       PIC S9(4) COMP.                     
           10 NI-ABKJOBNAME         PIC S9(4) COMP.                     
           10 NI-ABKLFDNR           PIC S9(4) COMP.                     
           10 NI-ABKPARTNERDATUM    PIC S9(4) COMP.                     
           10 NI-ABKKATALOGDATUM    PIC S9(4) COMP.                     
           10 NI-ABKBEGABLAUF       PIC S9(4) COMP.                     
           10 NI-ABKBEGAKTU         PIC S9(4) COMP.                     
           10 NI-ABKENDAKTU         PIC S9(4) COMP.                     
           10 NI-ABKBEGEINARB       PIC S9(4) COMP.                     
           10 NI-ABKENDEINARB       PIC S9(4) COMP.                     
           10 NI-LETZTEAENDERUNG    PIC S9(4) COMP.                     
           10 NI-ABKABGLSTRSATZKZ   PIC S9(4) COMP.
           10 NI-ABKERSTELLTTST     PIC S9(4) COMP.
        05 DCL-TBAT0042.                                                
           10 ABKMANDANTNR         PIC S9(9) USAGE COMP.                
           10 ABKPARTGESID         PIC S9(4) USAGE COMP.                
           10 ABKJOBNAME           PIC X(8).                            
           10 ABKLFDNR             PIC S9(9) USAGE COMP.                
           10 ABKPARTNERDATUM      PIC X(10).                           
           10 ABKKATALOGDATUM      PIC X(10).                           
           10 ABKBEGABLAUF         PIC X(26).                           
           10 ABKBEGAKTU           PIC X(26).                           
           10 ABKENDAKTU           PIC X(26).                           
           10 ABKBEGEINARB         PIC X(26).                           
           10 ABKENDEINARB         PIC X(26).                           
           10 LETZTEAENDERUNG      PIC X(26).                           
           10 ABKABGLSTRSATZKZ     PIC X(1).
           10 ABKERSTELLTTST       PIC X(26).
      ************************** ENDE-DCLGEN *************************  
