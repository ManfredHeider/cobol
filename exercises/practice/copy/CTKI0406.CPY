      *---------------------------------------------------------------* 
      *    Standard fuer alle Objekte unter PVCS VM                     
      *            Datum      :  07.02.2006                             
      *            Kategorie  :  Copystrecke                            
      *---------------------------------------------------------------* 
      *                                                                 
      *---------------------------------------------------------------* 
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH         
      *---------------------------------------------------------------* 
      *    $Workfile:   CTKI0406.CPY  $                                 00010000

      *    $Revision:   2.2  $                                          
      *                                                                 
      *    $Log:   L:/mf/CPY/CTKI0406.CPv  $                            
      *
      *   Rev 2.2   Oct 10 2007 10:17:36   t13ex50
      *AKI00630: Einmalbeitragsversicherungen in der AS-Club-Wertung
      *                                                                 
      *   Rev 2.1   Feb 07 2006 15:42:34   t13ex27                      
      *AKI00555: DB-Änderung OKI 02/2006                                
      *                                                                 
      *   Rev 2.0   Feb 07 2006 12:48:04   t13ex27                      
      *Archiverstellung                                                 
      *                                                                 
      *---------------------------------------------------------------* 
      ******************************************************************
      * DCLGEN TABLE EINMALZAHLUNG                                      
      ******************************************************************
CCC005*    EXEC SQL DECLARE EINMALZAHLUNG TABLE                         
CCC004*    ( EMZVERTRAGSID                  CHAR(13) NOT NULL,          
CCC004*      EMZVERTRAGSNR                  CHAR(15) NOT NULL,          
CCC004*      EMZKOMPLETTVERT                CHAR(1) NOT NULL,           
CCC004*      EMZABSCHLUSSVBNR               INTEGER NOT NULL,           
CCC004*      EMZZAHLUNGSWEISU               SMALLINT NOT NULL,          
CCC004*      EMZZAHLUNGSWEISN               SMALLINT NOT NULL,          
CCC004*      EMZERSTELLDATUM                DATE NOT NULL,              
CCC004*      EMZURSRVERTRBEG                DATE NOT NULL,              
CCC004*      EMZURSPVERTREND                DATE NOT NULL,              
CCC004*      EMZGESAMTBEITRAG               DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZMONATSBEITRAG               DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZLZTMONBEITRAG               DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON1                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON2                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON3                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON4                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON5                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON6                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON7                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON8                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON9                 DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON10                DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON11                DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZBEITRAGMON12                DECIMAL(11, 2) NOT NULL,    
CCC004*      EMZASCLUBMONLF                 SMALLINT,                   
CCC004*      EMZBEITRMONSTAND               SMALLINT,                   
CCC004*      LETZTEAENDERUNG                TIMESTAMP NOT NULL          
CCC003*    ) END-EXEC.                                                  
      ******************************************************************
      * COBOL DECLARATION FOR TABLE  EINMALZAHLUNG                      
      ******************************************************************
       01  EINMALZAHLUNG-GESAMT.                                        
         05 NI-EINMALZAHLUNG-TAB.                                       
           10 NI-EINMALZAHLUNG-ELE PIC S9(4) COMP OCCURS 27 TIMES.      
         05 NI-EINMALZAHLUNG REDEFINES NI-EINMALZAHLUNG-TAB.            
           10 NI-EMZVERTRAGSID        PIC S9(4) COMP.                   
           10 NI-EMZVERTRAGSNR        PIC S9(4) COMP.                   
           10 NI-EMZKOMPLETTVERT      PIC S9(4) COMP.                   
           10 NI-EMZABSCHLUSSVBNR     PIC S9(4) COMP.                   
           10 NI-EMZZAHLUNGSWEISU     PIC S9(4) COMP.                   
           10 NI-EMZZAHLUNGSWEISN     PIC S9(4) COMP.                   
           10 NI-EMZERSTELLDATUM      PIC S9(4) COMP.                   
           10 NI-EMZURSRVERTRBEG      PIC S9(4) COMP.                   
           10 NI-EMZURSPVERTREND      PIC S9(4) COMP.                   
           10 NI-EMZGESAMTBEITRAG     PIC S9(4) COMP.                   
           10 NI-EMZMONATSBEITRAG     PIC S9(4) COMP.                   
           10 NI-EMZLZTMONBEITRAG     PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON1       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON2       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON3       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON4       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON5       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON6       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON7       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON8       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON9       PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON10      PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON11      PIC S9(4) COMP.                   
           10 NI-EMZBEITRAGMON12      PIC S9(4) COMP.                   
           10 NI-EMZASCLUBMONLF       PIC S9(4) COMP.                   
           10 NI-EMZBEITRMONSTAND     PIC S9(4) COMP.                   
           10 NI-LETZTEAENDERUNG      PIC S9(4) COMP.                   
         05 DCL-EINMALZAHLUNG.                                          
           10 EMZVERTRAGSID        PIC X(13).                           
           10 EMZVERTRAGSNR        PIC X(15).                           
           10 EMZKOMPLETTVERT      PIC X(1).                            
           10 EMZABSCHLUSSVBNR     PIC S9(9) COMP.                      
           10 EMZZAHLUNGSWEISU     PIC S9(4) COMP.                      
           10 EMZZAHLUNGSWEISN     PIC S9(4) COMP.                      
           10 EMZERSTELLDATUM      PIC X(10).                           
           10 EMZURSRVERTRBEG      PIC X(10).                           
           10 EMZURSPVERTREND      PIC X(10).                           
           10 EMZGESAMTBEITRAG     PIC S9(9)V9(2) COMP-3.               
           10 EMZMONATSBEITRAG     PIC S9(9)V9(2) COMP-3.               
           10 EMZLZTMONBEITRAG     PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON1       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON2       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON3       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON4       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON5       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON6       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON7       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON8       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON9       PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON10      PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON11      PIC S9(9)V9(2) COMP-3.               
           10 EMZBEITRAGMON12      PIC S9(9)V9(2) COMP-3.               
           10 EMZASCLUBMONLF       PIC S9(4) COMP.                      
           10 EMZBEITRMONSTAND     PIC S9(4) COMP.                      
           10 LETZTEAENDERUNG      PIC X(26).                           
      ******************************************************************
