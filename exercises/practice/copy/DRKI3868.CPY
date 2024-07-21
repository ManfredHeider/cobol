      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:
      * AUFTRAG  ! DATUM  ! AENDERUNG                       ! NAME
      * ---------------------------------------------------------------
      * DBO-7910 !09.04.21! Neuerstellung                   ! U130685
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
      *   COPY-MEMBER FÜR DEBITKARTEN DER DEUTSCHEN BANK
      *   (Ausgabedatei der Ermittlung)
      *------------------------------------------------
      *   LÄNGE= 200 BYTES 
      *
      *****************************************************************
       01  'XX'-SATZ.
         05  'XX'-GES                      PIC  9(002).
      *      GESELLSCHAFT   = 48
         05  'XX'-VERARB-AK               PIC  X(001).
      *      VERARBEITUNGSKENNZEICHEN
         05  'XX'-KEY.
             10  'XX'-VNR                  PIC  X(015).
             10  'XX'-UKONTONR             PIC  X(002).
             10  'XX'-WAEHRUNG             PIC  X(003).
             10  'XX'-KARTENNR             PIC  X(013).
      *      VERTRAGSNUMMER = FILIALNR + KUNDENNR MIT FÜHRENDEN NULLEN
      *      UNTERKONTONR
      *      WAEHRUNGSSCHLUESSEL
      *      KARTENNR
         05  'XX'-KARTENART                PIC  X(003).          
      *      KARTENART
         05  'XX'-KARTENART-AK             PIC  X(001).
      *      ÄNDERUNGSKENNZEICHEN KARTENART
         05  'XX'-GUELTIG-BIS-DAT          PIC  9(004).          
      *      GÜLTIG BIS JJMM      
         05  'XX'-GUELTIG-BIS-DAT-AK       PIC  X(001).
      *      ÄNDERUNGSKENNZEICHEN GÜLTIGKEIT JJMM
         05  'XX'-INHNAME1                 PIC  X(030).
      *      NAME KARTENINHABER TEIL1
         05  'XX'-INHNAME1-AK              PIC  X(001).
      *      NAME KARTENINHABER TEIL1 KENNZEICHEN   
         05  'XX'-INHNAME2                 PIC  X(030).
      *      NAME KARTENINHABER TEIL2
         05  'XX'-INHNAME2-AK              PIC  X(001).
      *      NAME KARTENINHABER TEIL2 KENNZEICHEN 
         05  'XX'-NFCFAEHIG                PIC  X(001).
      *      NFC-FÄHIG
         05  'XX'-NFCFAEHIG-AK             PIC  X(001).
      *      NFC-FÄHIG KENNZEICHEN   
         05  'XX'-NFCSTATUS                PIC  X(001).
      *      NFC-STATUS
         05  'XX'-NFCSTATUS-AK             PIC  X(001).
      *      NFC-STATUS KENNZEICHEN
         05  FILLER                        PIC  X(089).
      *      RESERVE
      
      
      
      
     