      *****************************************************************
      * <KURZE FACHLICHE BESCHREIBUNG>  
      * DEUTSCHE VERECHNUNGSSTELLE EOS (90) KIS (INKASSO)            *
      * AUTOMATISCHE MASCH. NACHERFASSUNG                            *
      ****************************************************************
      * COPY-MEMBER: DRAN0324                                        *
      * SATZBESCHREIBUNG                                             *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AN-898  !        !1.0 !NEU                        ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
       01  ESATZ-GES90-KIS.
           05 ESATZ-GES90-KIS-GESAMT                PIC X(350).
      *
           05 GES90-VORSATZ-KIS REDEFINES ESATZ-GES90-KIS-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-VORSATZ-KIS-SATZ-KZ          PIC X(001).
      **** ABSENDER
              10 GES90-VORSATZ-KIS-ABSENDER         PIC 9(004).
      **** DTA-NAME
              10 GES90-VORSATZ-KIS-DTA-NAME         PIC X(010).
      **** BEREITSTELLUNGSDATUM FORMAT TT.MM.JHJJ
              10 GES90-VORSATZ-KIS-DAT-BEREITST     PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(325).
      *
           05 GES90-ANTRAGSATZ-KIS REDEFINES ESATZ-GES90-KIS-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-ANTRAGSATZ-KIS-SATZ-KZ       PIC X(001).
      **** DSL-BANK-VERTRAGSNUMMER
              10 GES90-ANTRAGSATZ-KIS-SCB-VNR       PIC 9(015).
      **** ABSCHLUSSVERMITTLER
              10 GES90-ANTRAGSATZ-KIS-AVERM         PIC 9(007).
      **** VERTRAUENSMANN VM
              10 GES90-ANTRAGSATZ-KIS-VM            PIC 9(007).
      **** KUNDE NAME
              10 GES90-ANTRAGSATZ-KIS-KUNDE-NA      PIC X(030).
      **** KUNDE VORNAME
              10 GES90-ANTRAGSATZ-KIS-KUNDE-VNA     PIC X(030).
      **** KUNDE PLZ
              10 GES90-ANTRAGSATZ-KIS-KUNDE-PLZ     PIC X(009).
      **** KUNDE ORT
              10 GES90-ANTRAGSATZ-KIS-KUNDE-ORT     PIC X(030).
      **** KUNDE LKZ
              10 GES90-ANTRAGSATZ-KUNDE-KIS-LKZ     PIC X(003).
      **** DVAG-PRODUKT-SATZ
              10 GES90-ANTRAGSATZ-KIS-PRODUKTE OCCURS 9.
      **** DVAG-PRODUKT
                 15 GES90-ANTRAGSATZ-KIS-PRODUKT    PIC 9(004).
      **** DVAG-VERTRAGSSUMME
                 15 GES90-ANTRAGSATZ-KIS-SUMME      PIC S9(012).
      **** DATUM ERFASSUNG BEI DSL(ERFASSUNGS-DATUM) FORMAT TT.MM.JHJJ
              10 GES90-ANTRAGSATZ-KIS-ERF-DATUM     PIC X(010).
      **** FILLER
              10 FILLER                             PIC X(64).

           05 GES90-NACHSATZZ-KIS REDEFINES ESATZ-GES90-KIS-GESAMT.
      **** SATZKENNZEICHEN
              10 GES90-NACHSATZ-KIS-SATZ-KZ         PIC X(001).
      **** ANZAHL SÄTZE INKL. VOR- UND NACHSATZ
              10 GES90-NACHSATZ-KIS-SATZANZAHL      PIC 9(007).
      **** FILLER
              10 FILLER                             PIC X(342).

      ******** ENDE COPY-MEMBER **************************************



       