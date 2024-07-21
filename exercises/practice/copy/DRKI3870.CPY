      *AUTHOR.        Thorsten Flierl.
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG  ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO8382  !30.04.21! Neuerstellung                   ! t13ex78
      * ---------!--------!---------------------------------!----------
      * DBO-10479!10.10.22! Felder INHNAME1 und INHNAME2    ! u130684  
      *          !        ! von 23 auf 30 erweitert         !
      *          !        ! Feld NFCSTATUS neu hinzugefügt  !
      * ---------!--------!---------------------------------!----------
      *
      * ENDE-LOGBUCH
      *****************************************************************
      *****************************************************************
      *
      *   G48 Debitkarten                 
      *   Copystrecke Eingabe Aktualisierungs-PGM PMKI1557
      *
      *   Satzlänge - 200 Bytes
      *
      *****************************************************************
      *
       01  'XX'-SATZ.
      *
      * Änderungskennzeichen - 'I', 'U' oder 'D'
           05  'XX'-AKTUAL-AK                PIC  X(001).
      *
      * Gesellschaft
           05  'XX'-GES                      PIC  X(002).
      *    
      * Ordnungsbegriff    
           05  'XX'-KEY.
      *    Vertragsnummer mit führenden Nullen    
               10  'XX'-VNR                  PIC  X(015).
      *    Unterkontonummer - entspr. BKPLFDNR   
               10  'XX'-UKONTONR             PIC  9(004).
      *    Währung (immer EUR)        
               10  'XX'-WAEHRUNG             PIC  X(003).
      *    Kartennummer        
               10  'XX'-KARTENNR             PIC  X(013).
      *        
      * Kartenart - https://projektportal.dvag.com/confluence/pages/
      * viewpage.action?pageId=394209922
           05  'XX'-KARTENART                PIC  X(003).
      *
      * Gültig bis im Format JJMM
           05  'XX'-GUELTIG-BIS              PIC  X(004).
      *
      * Name des Karteninhabers Teil 1 (Freitext bei der DeuBa)
           05  'XX'-INHNAME1                 PIC  X(030).
      *
      * Name des Karteninhabers Teil 2 (Freitext bei der DeuBa)
           05  'XX'-INHNAME2                 PIC  X(030).
      *
      * Karte ist NFC-fähig? 'I', 'J', 'N' oder Leer
           05  'XX'-NFCFAEHIG                PIC  X(001).
      *
      * NFC-Status - 'I', 'A', 'D' oder Leer
           05  'XX'-NFCSTATUS                PIC  X(001).
      * 
      * Reserve
           05  FILLER                        PIC  X(093).
               