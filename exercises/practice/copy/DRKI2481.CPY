      *****************************************************************
      *    COPY-MEMBER: DRKI2481                                      
      *                                                               
      *    G21/VIRIDIUM DTA-VERTRAG                                   
      *                                                               
      *    DTA VERTRAG ZUM PG G21/PROXALTO ÜBER DIE LIEFERUNG DER     
      *    VIRIDIUM.                                                  
      *                                                               
      *    SATZLÄNGE VARIABEL
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     
      * --------------------------------------------------------------
      * DBO-10293  !15.08.22! NEU                          ! T13AC20  
      * -----------!--------!------------------------------!----------
      *            !        !                              !          
      *            !        !                              !          
      *                                                               
      * ENDE-LOGBUCH                                                  
      *****************************************************************
      *
      *    HAUPTSATZ SATZART 'C'
           05 HAUPTSATZ.
              10 SATZART                       PIC  X(001).
              10 VNR                           PIC  X(015).
              10 VBNR                          PIC  9(007).
              10 SPARTE                        PIC  X(003).
              10 AENDKZN                       PIC  X(001).
              10 VERTRZUSTAND                  PIC  9(002).
              10 VERTRBEGINN                   PIC  X(010).
              10 VERTRABLAUF                   PIC  X(010).
              10 ZAHLBEITRAG                   PIC  9(008)V99.
              10 NJBEITRAG                     PIC  9(008)V99.
              10 ZAHLWEISE                     PIC S9(002).
              10 PRODUKT                       PIC  X(050).
              10 TARIF                         PIC  X(050).
              10 RENTENBEZUG                   PIC  X(001).
              10 RENTEVORAUS                   PIC  9(006).
              10 RENTEGARANT                   PIC  9(006).
              10 RNTZAHLW                      PIC S9(002).
              10 LEISTUNG-KZN                  PIC  X(001).
              10 BERECH-DAT                    PIC  X(010).
              10 ERLFALL-SUM                   PIC  9(008)V99.
              10 TODFALL-SUM                   PIC  9(008)V99.
              10 RNTFAKTOR                     PIC  9(005)V99.
              10 KAPABFIND                     PIC  9(008)V99.
              10 FONDSGUTH                     PIC  9(008)V99.
      *
      *    VORSATZ   SATZART 'A'
           05 VORSATZ  REDEFINES HAUPTSATZ.
              10 V-SATZART                     PIC  X(001).
      *          ABSENDER FIX 'VIR'
              10 V-ABSENDER                    PIC  X(003).
              10 V-ERSTELLDAT                  PIC  X(010).
      *          EMPFAENGER FIX 'ALAG'
              10 V-EMPFAENGER                  PIC  X(004).
      *          INHALT FIX 'VERTRAG             '
              10 V-INHALT                      PIC  X(020).
      *
      *    NACHSATZ  SATZART 'E'
           05 NACHSATZ REDEFINES HAUPTSATZ.
              10 N-SATZART                     PIC  X(001).
              10 N-ANZSATZ                     PIC  9(009).
      *****************************************************************
      *---------------------- ENDE DRKI2481 --------------------------* 
      *****************************************************************