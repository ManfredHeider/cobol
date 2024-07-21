      *---------------------------------------------------------------*
      * DVAG-Hyp(61) SST AUTOMATISCHE MASCH. NACHERFASSUNG
      ****************************************************************
      *    COPY-MEMBER: DRAN0356                                     *
      *    SATZBESCHREIBUNG                                          *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAN01434!25.10.19!2.1 !NEU                        ! Töpfer   *
      *         !        !    !                           !          *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * ANTRAG-1806!25.10.22! PRÜFUNG AUF DOPPELTVERARB.   ! RAHNER   *
      *            !        !                              !          *
      *            !        !                              !          *
      *            !        !                              !          *
      * ENDE-LOGBUCH                                                  *
      ****************************************************************
       01  ESATZ-GES61.
           05 ESATZ-GES61-GESAMT                    PIC X(226).
      *
           05 GES61-VORSATZ REDEFINES ESATZ-GES61-GESAMT.
      **** SATZKENNZEICHEN
              10 GES61-VORSATZ-SATZ-KZ              PIC X(001).
      **** ABSENDER
              10 GES61-VORSATZ-ABSENDER             PIC X(008).
      **** EMPFÄNGER
              10 GES61-VORSATZ-EMPFAENGER           PIC X(004).
      **** INHALT
251022*       10 GES61-VORSATZ-INHALT               PIC X(021).
251022        10 GES61-VORSATZ-INHALT               PIC X(011).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES61-VORSATZ-DAT-BEREITST         PIC X(010).
      **** WÄHRUNG
              10 GES61-VORSATZ-WAEHRUNG             PIC X(003).
      **** FILLER
251022*       10 FILLER                             PIC X(179).
251022        10 FILLER                             PIC X(189).
      *
           05 GES61-ANTRAGSATZ REDEFINES ESATZ-GES61-GESAMT.
      **** SATZKENNZEICHEN
              10 GES61-ANTRAGSATZ-SATZ-KZ           PIC X(001).
      **** GESELLSCHAFT
              10 GES61-GESELLSCHAFT                 PIC 9(002).
      **** DVAG-HYP-VERTRAGSNUMMER
              10 GES61-ANTRAGSATZ-DSL-VNR           PIC 9(015).
      **** VERTRAGSBEGINN
              10 GES61-VERTRAG-BEGINN               PIC X(010).
      **** FÄLLIG-VON
              10 GES61-FAELLIG-VON                  PIC X(010).
      **** ABSCHLUSSVERMITTLER
              10 GES61-ANTRAGSATZ-AVERM             PIC 9(007).
      **** KUNDE NAME
              10 GES61-ANTRAGSATZ-KUNDE-NAME        PIC X(035).
      **** KUNDE VORNAME
              10 GES61-ANTRAGSATZ-KUNDE-VNAME       PIC X(035).
      **** ABRECHNUNGSDATUM PG
              10 GES61-ABRECHDATUM-PG               PIC X(010).
      **** WÄHRUNG
              10 GES61-WAEHRUNG                     PIC X(003).
      **** ZAHLWEISE
              10 GES61-ZAHLWEISE                    PIC X(002).
      **** DVAG-PRODUKT
              10 GES61-ANTRAGSATZ-PROD-VORFALL      PIC 9(004).
      **** DVAG-PRODUKTBEZEICHNUNG
              10 GES61-ANTRAGSATZ-PROD-BEZ          PIC X(012).
      **** DVAG-VERTRAGSSUMME
              10 GES61-ANTRAGSATZ-SUMME             PIC 9(010).
      **** PROVISIONSSATZ
              10 GES61-PROVISION-SATZ               PIC 9(006).
      **** PROVISIONSBETRAG
              10 GES61-PROVISION-BETRAG             PIC 9(009).
      **** ABSCHLUSSVERMITTLER-VM
              10 GES61-ANTRAGSATZ-AVERM-VM          PIC 9(007).
      **** KUNDE PLZ
              10 GES61-ANTRAGSATZ-KUNDE-PLZ         PIC X(005).
      **** KUNDE WOHNORT
              10 GES61-ANTRAGSATZ-KUNDE-ORT         PIC X(030).
      **** KUNDE LÄNDERKENNZEICHEN
              10 GES61-ANTRAGSATZ-KUNDE-AUSL        PIC X(003).
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES61-ANTRAGSATZ-ERF-DATUM         PIC X(010).

           05 GES61-NACHSATZ REDEFINES ESATZ-GES61-GESAMT.
      **** SATZKENNZEICHEN
              10 GES61-NACHSATZ-SATZ-KZ             PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES61-NACHSATZ-SATZANZAHL          PIC 9(007).
      **** Summe aller Provisionsgutschriften
              10 GES61-NACHSATZ-SUM-POVG            PIC 9(017).
      **** Summe aller Provisionsbelastungen
              10 GES61-NACHSATZ-SUM-POVB            PIC 9(017).
      **** FILLER
              10 FILLER                             PIC X(184).
 

      ******** ENDE COPY-MEMBER **************************************







