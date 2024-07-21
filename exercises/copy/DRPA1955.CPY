      *****************************************************************
      *    COPY-MEMBER: DRPA1955                                     
      *                                                              
      *   <KURZE FACHLICHE BESCHREIBUNG>                             
      *    SCHNITTSTELLE FÜR EBASE KOSTENAUSWEIS ZWISCHEN DAISY      
      *    UND ITOPEA                                                
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *                                                              
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DAISY-3607 !01.06.21! NEU                          ! T13AE37  *
      * DAISY-5032 !16.11.22! PROZENT-AUSGABE VERGRÖSSERN  ! T13AE37  *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           05  KOSTENAUSWEIS.
               10 DEPOT-PREFIX             PIC  X(003) VALUE "991".
               10 DEPOT-NR                 PIC  X(010).
               10 FILLER                   PIC  X(001) VALUE ';'.

               10 ZEITRAUM.
                  15 VONDAT                PIC  X(006).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 BISDAT                PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 JAHR                  PIC  X(004).
                  15 FILLER                PIC  X(001) VALUE ';'.

      **** KUNDENDATEN
               10 KUNDE.
                  15 ADRZEILE1             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE2             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE3             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE4             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE5             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE6             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE7             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRZEILE8             PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
          
      **** AUSWEISE FÜR FINANZINSTRUMENTE
      *        
               10 FINANZINSTRUMENT.      
                  15 PROD-BEZ              PIC  X(070).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ISIN                  PIC  X(012).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 WKN                   PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              DIENSTLEISTUNGSKOSTEN (H)
                  15 FOND-DL-KOSTEN        PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              DIENSTLEISTUNGSPROZENTE (H1)
                  15 FOND-DL-PROZ          PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              PRODUKTKOSTEN (J)
                  15 FOND-PR-KOSTEN        PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              PRODUKTKOSTENPROZENTE (J1)
                  15 FOND-PR-PROZ          PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              PROVISION FÜR ISIN (I)
                  15 FOND-PROVISION        PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              PROVISION - PROZENTE FÜR ISIN (I1)
                  15 FOND-PROVISION-PROZ   PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              SUMME AUS PRODUKT UND DIENSTLEISTUNGSKOSTEN (K)
                  15 FOND-GSMT             PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              PROZENTE AUS PRODUKT UND DIENSTLEISTUNGSKOSTEN (K1)
                  15 FOND-GSMT-PROZ        PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      
      **** AUSWEISE FÜR DEPOT
               10 DEPOT.
      *              DURCHSCHNITTSVOLUMEN (S)
                  15 DPT-VOLUMEN           PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** DEPOTGEBÜHREN (L)
                  15 DEPOT-GEBUEHR         PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** DEPOTGEBÜHREN (L1)
                  15 DEPOT-GEBUEHR-PROZ    PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** SUMME AUS ALLEN DIENSTLEISTUNGSKOSTEN + DEPOTGEBÜHR (M1)
                  15 DPT-DL-KOSTEN         PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              ANTEIL IN % (M2)
                  15 DPT-DL-PROZ           PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** SUMME AUS PROVISIONEN  (N1) 
                  15 DPT-PROV              PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              ANTEIL IN % (N2)
                  15 DPT-PROV-PROZ         PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** SUMME ALLER PRODUKTKOSTEN (O1)
                  15 DPT-PR-KOSTEN         PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              ANTEIL IN % (O2)
                  15 DPT-PR-PROZ           PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.

      *** GESAMTKOSTEN FÜR DEPOT (P1)
                  15 DPT-GSMT              PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *              ANTEIL IN % (P2)
                  15 DPT-GSMT-PROZ         PIC  X(008).
                  15 FILLER                PIC  X(001) VALUE ';'.

      *** SUMME EINMALIGE DIENSTLEISTUNGSKOSTEN (Q)
                  15 DPT-DL                PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.

      *** SUMME LAUFENDE KOSTEN (R)
                  15 DPT-KOSTEN-LFD        PIC  X(014).
                  15 FILLER                PIC  X(001) VALUE ';'.


      *** Betreueradresse
               10 ADRESSE.
                  15 ADRESSZEILE_1         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_2         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_3         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_4         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_5         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_6         PIC  X(030).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_7         PIC  X(030).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_8         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_9         PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_10        PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ADRESSZEILE_11        PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  
      *** KUNDENBETREUER
               10 VB-NR                    PIC  X(009).
               10 FILLER                   PIC  X(001) VALUE ';'.
      *** DAISY-VNR INKL. UNTERDEPOT
               10 DAISY-VNR                PIC  X(015).
               10 FILLER                   PIC  X(001) VALUE ';'.

      *****************************************************************
      *---------------------- ENDE DRPA1955 --------------------------* 
      *****************************************************************
