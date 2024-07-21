      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG  ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO-7910 !09.04.21! Neuerstellung                   ! U130685
      * ---------!--------!---------------------------------!----------
      * DBO-10479!10.10.22! Felder INHNAME1 und INHNAME2    ! u130684
      *          !        ! von 23 auf 27 erweitert         !
      *          !        ! Feld NFCSTATUS neu hinzugefügt  !
      * ---------!--------!---------------------------------!----------
      *          !        !                                 !
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   COPY-MEMBER FÜR DEBITKARTEN DER DEUTSCHEN BANK
      *   (Eingabedatei der Ermittlung)
      *------------------------------------------------
      *   LÄNGE= 100 BYTES                               
      *
      *****************************************************************
       01  DRKI3867.                                                  
           10  'XX'-VORSATZ.                                          
      *                                                         
                20  'XX'-V-SA            PIC X(002).                   
      *              SATZART = '01'                                     
                20  'XX'-V-ABSENDER      PIC X(008).                   
      *              ABSENDER = 'DB AG'                                 
                20  'XX'-V-DATUM         PIC 9(008).                   
      *              BUCHUNGSDATUM = 'JJJJMMTT'                         
                20  'XX'-V-EMPFAENGER    PIC X(004).                   
      *              EMPFAENGER = 'DVAG'                                
                20  'XX'-V-TEXT          PIC X(008).                   
      *              INHALTSANGABE:                                     
      *              DEBITKARTEN  TÄGLICH    DKARTET'                  
      *              DEBITKARTEN  KOMPLETT   DKARTEK'                  
                20  'XX'-V-WKZ           PIC X(003).                   
      *              WÄHRUNGS-KENNZ. =  'EUR'                           
                20   FILLER              PIC X(067).                    
      *              RESERVE                                            
                                                                
                                                                
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.             
      *                                                         
                20  'XX'-SA              PIC X(002).          
      *              SATZART = '24'                              
                20  'XX'-GES             PIC 9(002).          
      *              GESELLSCHAFT = '48'                              
                20  'XX'-VNR.           
                     25  'XX'-FILIALNR PIC 9(003).
                     25  'XX'-KUNDENNR PIC 9(007).
                     25  'XX'-UKONTONR PIC 9(002).
                     25  'XX'-WAEHRUNG PIC 9(003).
      *              VERTRAGSNUMMER =
      *              FILIALNR
      *              KUNDENNR
      *              UNTERKONTONR
      *              WAEHRUNGSSCHLUESSEL
                20  'XX'-KARTENART       PIC X(003).          
      *              KARTENART
                20  'XX'-KARTENNR        PIC X(013).          
      *              KARTENNUMMER                                
                20  'XX'-GUELTIG-BIS-DAT PIC 9(004).          
      *              GÜLTIG BIS JJMM      
                20  'XX'-DELKZ           PIC X(001).         
      *              DELETE KENNZEICHEN               
                20  'XX'-INHNAME1        PIC X(027).
      *              NAME KARTENINHABER TEIL1
                20  'XX'-INHNAME2        PIC X(027).
      *              NAME KARTENINHABER TEIL2
                20  'XX'-NFCFAEHIG       PIC X(001).
      *              NFC-FÄHIG
                20  'XX'-NFCSTATUS       PIC X(001).
      *              NFC-STATUS
                20  FILLER               PIC X(004).
      *    
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *                                              
                20  'XX'-N-SA            PIC X(002).        
      *              SATZART = '99'                 
                20  'XX'-N-ANZ           PIC 9(007).        
      *              ANZAHL SAETZE OHNE VOR- UND NACHSATZ   
                20  FILLER               PIC X(091).         
      *              RESERVE                                 

