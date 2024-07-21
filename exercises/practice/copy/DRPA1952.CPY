      *****************************************************************
      *    COPY-MEMBER: DRPA1952                                     
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
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           05  EBASE-DATEN.
               10 DEPOT-PREFIX             PIC  X(003) VALUE "991".
               10 DEPOT-NR                 PIC  X(010).
               10 FILLER                   PIC  X(001) VALUE ';'.
               10 VONDAT                   PIC  X(010).
               10 FILLER                   PIC  X(001) VALUE ';'.
               10 BISDAT                   PIC  X(010).
               10 FILLER                   PIC  X(001) VALUE ';'.

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

      **** GESAMTKOSTEN
               10 GESAMT.
                  15 GSMT-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GSMT-PROZ             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GSMT-BETR             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 INSTR-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 INSTR-PROZ            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 INSTR-BETR            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 DIENST-WHRG           PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 DIENST-PROZ           PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 DIENST-BETR           PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.

      **** RENDITE
               10 RENDITE.
                  15 GSMT-DRCHSCHNTT       PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GSMT-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GSMT-PROZ             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GSMT-BETR             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 KONTO-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 KONTO-PROZ            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 KONTO-BETR            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 FONDS-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 FONDS-PROZ            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 FONDS-BETR            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.

      **** ZUWENDUNGEN
               10 ZUWENDUNG.
      *           VON DRITTEN AN EBASE
                  15 REIN-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 REIN-PROZ             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 REIN-BETR             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *           DAVON LAUFENDE VERTRIEBSPROVSION
                  15 REIN-P-WHRG           PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 REIN-P-BETR           PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *           VON EBASE AN DRITTE GEWÄHRT
                  15 RAUS-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 RAUS-PROZ             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 RAUS-BETR             PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *           DAVON LAUFENDE VERTRIEBSPROVSION
                  15 RAUS-LVP-WHRG         PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 RAUS-LVP-BETR         PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *           DAVON VERTRIEBSPROVSION
                  15 RAUS-VP-WHRG          PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 RAUS-VP-BETR          PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                
      **** DETAILS ZU
      *        
              10 FINANZINSTRUMENT.      
                  15 UNTERDEPOT            PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 ISIN                  PIC  X(012).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 DL-BETR               PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PR-BETR               PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PROD-BEZ              PIC  X(060).
                  15 FILLER                PIC  X(001) VALUE ';'.
      * 
      *         ERWERBSKOSTEN
      *          DAVON PROVISION
                  15 PROV-DL-BETR          PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PROV-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *          DAVON SONSTIGE
                  15 SONST-DL-BETR         PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 SONST-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *         LAUFENDE KOSTEN
      *          DAVON LAUFENDE VERTRIEBSPROVISION
                  15 VPROV-DL-BETR         PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 VPROV-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *          DAVON PERIODENBEZOGENE GESAMTKOSTEN
                  15 PGSMT-PR-BETR         PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PGSMT-WHRG            PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *          ABZÜGLICHE LAUFENDE VERTRIEBSPROVISION
                  15 LFDV-PR-BETR          PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 LFDV-WHRG             PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *         VERÄUSSERUNGSKOSTEN
      *          DAVON SONSTIGE
                  15 VSONST-DL-BETR        PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 VSONST-WHRG           PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *          DAVON RÜCKNAHMEKOSTEN
                  15 VRUECK-PR-BETR        PIC  X(010).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 VRUECK-WHRG           PIC  X(003).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** PROV-DATEN AUS DAISY
      *           JE FINANZINSTRUMENT
               10 PROVISIONS-DATEN.
                  15 DAISY-TARIF           PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PROV-BETRAG           PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 DAISY-VNR             PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *           SUMME ALLER PROVISIONEN
                  15 DAISY-PROV-SUMME      PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** GESAMTSUMMEN PRODUKT UND DIENSTLEISTUNGSKOSTEN
               10 EBASE-SUMMEN.
                  15 DL-SUMME              PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 PR-SUMME              PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
                  15 GESAMTKOSTEN          PIC  X(015).
                  15 FILLER                PIC  X(001) VALUE ';'.
      *** KUNDENBETREUER
               10 VB-NR                    PIC  9(009).

      *****************************************************************
      *---------------------- ENDE DRPA1952 --------------------------* 
      *****************************************************************
