      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2581.CPY  $
      *    $Revision:   2.6  $
      *
      *    $Log:   L:/mf/CPY/DRKI2581.CPv  $
      *
      *   Rev 2.6   Sep 04 2020 11:58:08   t13ac20
      *AKI01799: Central: Aufnahme des EBE Beitrags in die Bestands
      *daten - Source
      *
      *   Rev 2.5   Mar 06 2018 10:13:36   t13ac13
      *AKI01568: Central: Bestandserweiterung um ein Kennz. für Anw
      *artschaft - Copys
      *
      *   Rev 2.4   Mar 27 2017 09:29:22   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.3   Sep 12 2016 11:03:58   t13ex78
      *AKI01463: Programme, Copies ect.
      *
      *   Rev 2.2   Jun 13 2013 14:51:22   t13ae31
      *AKI01204: SEPA - DB-Online-CKV
      *
      *   Rev 2.1   Jun 26 2012 17:04:08   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 14 2012 15:51:48   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV direkte Datenlieferung   *
      *    vor der Sortierung.                                         *
      *    -----------------------------------                         *
      *    SATZLAENGE 340                                              *
      *                                                                *
      *    ANGELEGT 05.04.2012     / Schlenther                        *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         ! Name          *
      *AUFTRAG !                                       !               *
      *--------!---------------------------------------!---------------*
      *05.04.12! Neuerstellung wegen neuer             ! Schlenther    *
      *AKI01100! Schnittstelle.                        !               *
      *--------!---------------------------------------!---------------*
      *15.06.13! SEPA                                  ! Hübner        *
      *AKI01204!                                       !               *
      *--------!---------------------------------------!---------------*
250816*AKI01463! Fremdversicherer und Korrespondenz-   ! tfl           *
      *        ! makler                                !               *
      *--------!---------------------------------------!---------------*
      *AKI01544! FVD entfernt                          ! HHU           *
      *        !                                       !               *
      *--------!---------------------------------------!---------------*
160318*AKI01568! Kennzeichen STVTBTG (Anwartschaft) neu! Boost         *
      *--------!---------------------------------------!---------------*
      *AKI01799! neues Feld  BE-REDUK-BTG              ! T13AC20       *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      ******************************************************************
       01  DRKI2581.
      ** DTA VORSATZ / A-Satz wegen Kennzeichnung Gesamt- bzw.
      ** Aenderungsbestand. Kann nur 1 x je DTA vorhanden sein.
           05  CKV-SATZ0.
             10  CKV-SAA                 PIC X(01).
      ** 'A' Konstant
             10  CKV-SAA-ABSENDER        PIC X(27).
      ** 'CENTRAL KRANKENVERS KOELN'
             10  CKV-SAA-ERSTELLDATUM    PIC 9(08).
      ** ErstellDatum in der Form TTMMJJJJ
             10  CKV-SAA-DTASTAND        PIC 9(08).
      ** Stichtag in der Form TTMMJJJJ
             10  CKV-SAA-EMPFAENGER1     PIC X(04).
      ** 'DVAG'
      ** 'ALLF'
             10  CKV-SAA-EMPFAENGER2     PIC X(23).
      ** 'FFM-RECHENZENTRUM-5.OG'
             10  CKV-SAA-DTA-VTRG        PIC X(11).
      ** 'DTA-VERTRAG' Konstanter Inhalt.
             10  CKV-SAA-BESTAND-ART     PIC X(05).
      ** 'KOMPL' oder 'AEND'
             10  FILLER                  PIC X(04).
             10  CKV-SAA-WHRG-KNZ        PIC X(03).
             10  CKV-SAA-RESERVE0        PIC X(246).
      **
      ** Vertragssatz C2891 (neu: Loeschkennzeichen - noetig nach Um-
      ** stellung auf Deltalieferung; Es wird bei Loeschung nur noch der
      ** Vetragssatz ausgeliefert, keine Personen- und Tarifsätze und
      ** kein Banksatz
      ** / Gebdat VN: aus Platzgruenden nur 8 statt 10 Stellen;
      ** Punkte beim Datum weggelassen!)
      **
      ** Vertragssatz C2891 wegen Loeschkennzeichen - noetig nach Um-
      ** stellung auf Deltalieferung.
      ** Kann nur 1 x je VertragsNr. vorhanden sein.
           05  CKV-SATZ1 REDEFINES CKV-SATZ0.
            10  CKV-SA1-KEY.
              15  CKV-SA1-SAZDTA          PIC X(01).
      **     'C' Konstant
              15  CKV-SA1-SAZART          PIC X(03).
      **     '289' Konstant
              15  CKV-SA1-SSAZFUG         PIC X(01).
      **     '1' Konstant
              15  CKV-SA1-GESCHAEFTST     PIC X(03).
      **     CKV-Geschäftsstelle
              15  CKV-SA1-VTRGNR          PIC 9(09).
      **     Vertragsnummer
              15  CKV-SA1-PERSNR          PIC 9(04).
      **     PersonenNummer
            10 CKV-SA1-VERTRAGSSSATZ.
              15  CKV-SA1-ITVT            PIC 9(09).
      **     TV-Teil
              15  CKV-SA1-TTARNAM         PIC X(08).
      **     TarifName
              15  CKV-SA1-SONDERBETREUUNG PIC X(11).
              15  CKV-SA1-PARTINF         PIC X(15).
      ** PartnerInfo
      ** Ab der Stelle 8 in der Laenge 6 und aufgefuellt mit einer Null,
      ** so wird im AufbereitungsPGM die DVAG VB-Nummer ermittelt.
              15  CKV-SA1-VTRGBEGINN      PIC X(10).
              15  CKV-SA1-VTRGENDE        PIC X(10).
              15  CKV-SA1-VTRGAENDERUNG   PIC X(10).
      ** Aenderungs-/ErstellungsDatum des Vertragssatzes.
              15  CKV-SA1-ANREDE          PIC X(02).
              15  CKV-SA1-NAME            PIC X(40).
              15  CKV-SA1-TITEL1          PIC X(25).
              15  CKV-SA1-ADELSTITEL      PIC X(20).
              15  CKV-SA1-VORNAMEVN       PIC X(25).
              15  CKV-SA1-LANDKNZ         PIC X(03).
              15  CKV-SA1-PLZ             PIC X(06).
              15  CKV-SA1-ORT             PIC X(34).
              15  CKV-SA1-ORTZUSATZKNZ    PIC X(01).
              15  CKV-SA1-STRASSE         PIC X(40).
              15  CKV-SA1-BERUF-1         PIC 9(04).
              15  CKV-SA1-BERUF-2         PIC 9(09).
              15  CKV-SA1-LSV-J-N         PIC X(01).
              15  CKV-SA1-ZAHLUNGSWEISE   PIC X(01).
              15  CKV-SA1-PERSONENNR-VN   PIC 9(04).
              15  CKV-SA1-WHRG-KNZ        PIC X(03).
              15  CKV-SA1-COBA-KUNDENNR   PIC X(10).
      **     Coba-Kundennummer
              15  CKV-SA1-ABVERM          PIC 9(09).
      **     Vermittler-Nr. CKV-Eigene Vermittlernummer
              15  CKV-SA1-GEBDAT-VN       PIC X(08).
      ** Geburtsdatum VN i.d.F. JJJJMMDD(wird jetzt NEU mit uebernommen)
              15  CKV-SA1-LOESCHKNZ       PIC 9(01).
      **     '9' fuer Loeschung sonst '0'
      **
      ** Personensatz C2892
      ** Kann pro Vertragssatz 1:n vorhanden sein.
           05  CKV-SATZ2 REDEFINES CKV-SATZ0.
            10  CKV-SA2-KEY.
              15  CKV-SA2-SAZDTA          PIC X(01).
      ** 'C' Konstant
              15  CKV-SA2-SAZART          PIC X(03).
      ** '289' Konstant
              15  CKV-SA2-SSAZFUG         PIC X(01).
      ** '2' Konstant
              15  CKV-SA2-GST             PIC X(03).
      ** CKV-Geschäftsstelle
              15  CKV-SA2-VTRGNR          PIC 9(09).
      ** Vertragsnummer
              15  CKV-SA2-IPERNR          PIC 9(04).
      ** Nummer der versicherten Person
              15  CKV-SA2-ITVT            PIC 9(09).
      ** TarifTeil
              15  CKV-SA2-TTARNAM         PIC X(08).
      ** TarifName
            10  CKV-SA2-PERSONENSATZ.
              15  CKV-SA2-VORNAME         PIC X(25).
      ** Vorname aus Partner
              15  CKV-SA2-GEBDAT          PIC X(10).
              15  CKV-SA2-GESCHL          PIC X(01).
              15  CKV-SA2-BERUF-1         PIC 9(04).
      ** Feld SANGSTD aus CKV-SA2-DTA Satzbeschreibung vom 14.03.12
              15  CKV-SA2-BERUF-2         PIC 9(09).
      ** Feld SBRFTES aus CKV-SA2-DTA Satzbeschreibung vom 14.03.12
              15  CKV-SA2-PERSBEG         PIC X(10).
      ** Datum VersicherungsBeginn für die Versicherte Person
              15  CKV-SA2-PERSABL         PIC X(10).
      ** Datum VersicherungsEnde für die Versicherte Person
              15  CKV-SA2-PERSAEND        PIC X(10).
      ** Laufdatum
              15  CKV-SA2-DTNSPR          PIC 9(04).
      ** Datenschutzsperre
              15  CKV-SA2-NAME            PIC X(40).
              15  CKV-SA2-ANREDE          PIC X(01).
              15  CKV-SA2-TITEL2          PIC X(25).
              15  CKV-SA2-STRASSE         PIC X(25).
              15  CKV-SA2-LANDKNZ         PIC X(03).
              15  CKV-SA2-PLZ             PIC X(06).
              15  CKV-SA2-ORT             PIC X(25).
250816        15  CKV-SA2-VORVERSSCHL     PIC 9(05).
250816        15  CKV-SA2-VORVERSDATUM    PIC X(10).
250816        15  CKV-SA2-VORVERSKUENDKZ  PIC X(01).
250816*       15  CKV-SA2-RESERVE         PIC X(94).
250816        15  CKV-SA2-RESERVE         PIC X(78).
      ****
      ** DVAG-DTA Tarifsatz / C2893 bleibt unveraendert
      ** Kann pro Personensatz 1:n vorhanden sein.
           05  CKV-SATZ3 REDEFINES CKV-SATZ0.
            10  CKV-SA3-KEY.
              15  CKV-SA3-SAZDTA              PIC X(01).
      ** 'C' Konstant
              15  CKV-SA3-SAZART              PIC X(03).
      ** '289' Konstant
              15  CKV-SA3-SSAZFUG             PIC X(01).
      ** '3' Konstant
              15  CKV-SA3-GST                 PIC X(03).
      ** CKV-Geschäftsstelle
              15  CKV-SA3-VTRGNR              PIC 9(09).
      ** Vertragsnummer
              15  CKV-SA3-IPERNR              PIC 9(04).
      ** Nummer der versicherten Person
              15  CKV-SA3-ITVT                PIC 9(09).
      ** TarifTeil
              15  CKV-SA3-TTARNAM             PIC X(08).
      ** TarifName
            10  CKV-SA3-TARIFSATZ.
              15 CKV-SA3-ZUGANGVTRGSTAND      PIC X(10).
      ** Zugangsvertragsstand
              15 CKV-SA3-ZUGANGSERSTELLDAT    PIC X(10).
      ** Erstelldatum des Zugangs
              15 CKV-SA3-ABGANGSVTRGSTAND     PIC X(10).
      ** Abgangsvertragsstand
              15 CKV-SA3-TARNAM               PIC X(08).
      ** Tarifname
              15 CKV-SA3-TARFORM              PIC X(10).
      ** Tarifform
              15 CKV-SA3-VLEIST               PIC S9(9)V99.
      ** Versicherungsleistung
              15 CKV-SA3-SB-GESAMT            PIC S9(9)V99.
      ** Selbstbeteiligung GESAMT. Summe aus
      **                           BVSGSBG_IND,
      **                           BVSGSBG_TAR
      **                       und BVSGSBG_TAF (Bezeichnungen der CKV)
              15 CKV-SA3-BEITRAG              PIC S9(9)V99.
      ** Tarifbeitrag
160318        15 CKV-SA3-STVTBTG              PIC X(02).
160318** Kennzeichen Anwartschaft
              15 CKV-SA3-BE-REDUK-BTG         PIC S9(9)V99.
      ** Beitragsreduktion aus BE-Tarif
              15 CKV-SA3-RESERVE              PIC X(208).
      **
      ** Banksatz C2894 (komplett neu)
      ** DVAG-DTA BankSatz / C2894
      ** Wird dem VN (Satzart 1) zugerechnet und darf daher, wenn vor-
      ** handen nur einmal pro Vertragsnummer vorhanden sein.
      ** Banksatz kann, muss jedoch nicht vorhanden sein.
           05  CKV-SATZ4 REDEFINES CKV-SATZ0.
            10  CKV-SA4-KEY.
              15  CKV-SA4-SAZDTA              PIC X(01).
      ** 'C' Konstant
              15  CKV-SA4-SAZART              PIC X(03).
      ** '289' Konstant
              15  CKV-SA4-SSAZFUG             PIC X(01).
      ** '4' Konstant
              15  CKV-SA4-GST                 PIC X(03).
      ** CKV-Geschäftsstelle
              15  CKV-SA4-VTRGNR              PIC 9(09).
      ** Vertragsnummer
              15  CKV-SA4-IPERNR              PIC 9(04).
      ** Nummer der versicherten Person -immer leer-
              15  CKV-SA4-ITVT                PIC 9(09).
      ** TarifTeil -immer leer-
              15  CKV-SA4-TTARNAM             PIC X(08).
      ** TarifName -immer leer-
            10  CKV-SA4-BANKSATZ.
              15 CKV-SA4-LFD-BANK-NR          PIC 9(05).
      ** laufende Banknummer
              15 CKV-SA4-BANKTYP              PIC X(01).
      ** Bank-TYP
              15 CKV-SA4-BANKNAME             PIC X(30).
      ** Bankname
     ********* 15 CKV-SA4-KONTOINHABER         PIC X(40).
     **** Kontoinhaber
150613         15 CKV-SA4-NAME-BZ              PIC X(40).
     **** NAME abweichender Beitragszahler
              15 CKV-SA4-IBAN                 PIC X(34).
      ** IBAN / Kontonummer
      ** Die Kontonummer wird in seinem zukuenftigen Nachfolgerfeld
      ** IBAN geliefert. Mail von Herrn Chalupka(CENTRAL) vom
      ** 03.05.2012 um 14.11 Uhr.
              15 CKV-SA4-BIC                  PIC X(11).
      ** Bankinterne Clearing Nummer
              15 CKV-SA4-BLZ                  PIC X(08).
      ** BankLeitZahl -entfaellt in absehbarer Zeit-
150613******* neu für sepa
              15 CKV-VORNAME-BZ               PIC X(25).
      ** Vorname abweichender Beitragszahler
              15 CKV-STRASSE-BZ               PIC X(25).
      ** Strasse abweichender Beitragszahler
              15 CKV-LAND-BZ                  PIC X(03).
      ** Landkz abweichender Beitragszahler
              15 CKV-PLZ-BZ                   PIC X(06).
      ** PLZ abweichender Beitragszahler
              15 CKV-ORT-BZ                   PIC X(25).
      ** Ort abweichender Beitragszahler
              15 CKV-GEBDAT-BZ                PIC X(10).
      ** Geburtsdatum abweichender Beitragszahler
              15 CKV-ANREDE-BZ                PIC X(02).
      ** Anrede abweichender Beitragszahler
  *********   15 CKV-SA4-RESERVE              PIC X(173).
              15 CKV-SA4-RESERVE              PIC X(77).
      **
 
      ** Satz-DateiEnde
           05  CKV-SATZE REDEFINES CKV-SATZ0.
            10  CKV-SAE-ENDESATZ.
              15 CKV-SAE-KNZ          PIC X(01).
      ** 'E' Konstant
              15 CKV-SAE-GESAMT       PIC 9(07).
      ** Anzahl gelieferte Datensätze ohne Anfang- und Endesatz.
              15 CKV-SAE-RESERVE      PIC X(332).
      ******************************************************************
 
 


