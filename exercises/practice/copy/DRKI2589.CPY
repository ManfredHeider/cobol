      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.04.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2589.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2589.CPv  $
      *
      *   Rev 2.8   Jan 14 2021 15:12:14   t13ae31
      *AKI01823: G50 Vertragsanzeige Tarife mehrfach Pgme
      *
      *   Rev 2.7   Sep 04 2020 11:59:08   t13ac20
      *AKI01799: Central: Aufnahme des EBE Beitrags in die Bestands
      *daten - Source
      *
      *   Rev 2.6   Mar 06 2018 10:14:40   t13ac13
      *AKI01568: Central: Bestandserweiterung um ein Kennz. für Anw
      *artschaft - Copys
      *
      *   Rev 2.5   Mar 27 2017 09:29:30   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.4   Sep 12 2016 11:04:06   t13ex78
      *AKI01463: Programme, Copies ect.
      *
      *   Rev 2.3   Apr 06 2016 09:07:30   t13ex78
      *AKI01418: Überarbeitung PMKI0192 - doppelte Tarife
      *
      *   Rev 2.2   Jun 13 2013 14:51:28   t13ae31
      *AKI01204: SEPA - DB-Online-CKV'
      *
      *   Rev 2.1   Jun 26 2012 17:04:16   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Apr 05 2012 17:53:20   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV direkte Datenlieferung   *
      *    nach der Sortierung und erstellen neuer Datei durch OUTREC- *
      *    Befehl im Sortierjob Nr. 3                                  *
      *    -----------------------------------                         *
      *    SATZLAENGE 344                                              *
      *                                                                *
      *    ANGELEGT 05.04.2012     / Schlenther                        *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         ! Name          *
      *AUFTRAG !                                       !               *
      *--------!---------------------------------------!---------------*
      *26.04.12! Neuerstellung wegen neuer             ! Schlenther    *
      *AKI01100! Schnittstelle.                        !               *
      *--------!---------------------------------------!---------------*
      *15.06.13! SEPA                                  ! Hübner        *
      *AKI01204!                                       !               *
      *--------!---------------------------------------!---------------*
      *08.02.16! Copy-Strecke durch 'XX'-Präfix flex-  ! tfl           *
      *AKI01418! ibler gemacht. IN-Angabe in nutzenden !               *
      *        ! Programmen kann somit entfallen       !               *
      *--------!---------------------------------------!---------------*
250816*AKI01463! Fremdversicherer und Korrespondenz-   !               *
      *        ! makler                                ! tfl           *
      *--------!---------------------------------------!---------------*
      *FKI01544! FVD entfernt                          ! HHU           *
      *        !                                       !               *
      *--------!---------------------------------------!---------------*
160318*AKI01568! Kennzeichen STVTBTG (Anwartschaft)    ! Boost         *
      *        ! neu                                   !               *
      *--------!---------------------------------------!---------------*
      *AKI01799! neues Feld  BE-REDUK-BTG              ! T13AC20       *
      *--------!---------------------------------------!---------------*
      *AKI01823! neues Feld ANZ-TAR-DUP                ! HHU           *
      ******************************************************************
       01  'XX'-SATZ.
      ** DTA VORSATZ / A-Satz wegen Kennzeichnung Gesamt- bzw.
      ** Aenderungsbestand. Kann nur 1 x je DTA vorhanden sein.
           05  'XX'-SATZ0.
             10  'XX'-SAA                PIC X(01).
      ** 'A' Konstant
             10  'XX'-SAA-ABSENDER       PIC X(27).
      ** 'CENTRAL KRANKENVERS KOELN'
             10  'XX'-SAA-ERSTELLDATUM   PIC 9(08).
      ** ErstellDatum in der Form TTMMJJJJ
             10  'XX'-SAA-DTASTAND       PIC 9(08).
      ** Stichtag in der Form TTMMJJJJ
             10  'XX'-SAA-EMPFAENGER1    PIC X(04).
      ** 'DVAG'
      ** 'ALLF'
             10  'XX'-SAA-EMPFAENGER2    PIC X(23).
      ** 'FFM-RECHENZENTRUM-5.OG'
             10  'XX'-SAA-DTA-VTRG       PIC X(11).
      ** 'DTA-VERTRAG' Konstanter Inhalt.
             10  'XX'-SAA-BESTAND-ART    PIC X(05).
      ** 'KOMPL' oder 'AEND'
             10  FILLER                  PIC X(04).
             10  'XX'-SAA-WHRG-KNZ       PIC X(03).
             10  'XX'-SAA-RESERVE0       PIC X(250).
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
           05  'XX'-SATZ1 REDEFINES 'XX'-SATZ0.
            10  'XX'-SA1-KEY.
              15  'XX'-SA1-SAZDTA         PIC X(01).
      **     'C' Konstant
              15  'XX'-SA1-SAZART         PIC X(03).
      **     '289' Konstant
              15  'XX'-SA1-VTRGNR         PIC 9(09).
      **     Vertragsnummer
              15  'XX'-SA1-PERSNR         PIC 9(04).
      **     PersonenNummer
              15  'XX'-SA1-PERSNR-LFDNR   PIC 9(04).
      **      15  'XX'-SA1-ITVT1-4        PIC 9(04).
      **     TV-Teil Byte 1-4; Person laufende Nr.
              15  'XX'-SA1-TTARNAM        PIC X(08).
      **     TarifName
              15  'XX'-SA1-SSAZFUG        PIC X(01).
      **     '1' Konstant
              15  'XX'-SA1-GESCHAEFTST    PIC X(03).
      **     'XX'-Geschäftsstelle
              15  'XX'-SA1-ITVT           PIC 9(09).
      **     TV-Teil
      ******
            10 'XX'-SA1-VERTRAGSSSATZ.
              15  'XX'-SA1-SONDERBETREUUNG PIC X(11).
              15  'XX'-SA1-PARTINF        PIC X(15).
              15  'XX'-SA1-VTRGBEGINN     PIC X(10).
              15  'XX'-SA1-VTRGENDE       PIC X(10).
              15  'XX'-SA1-VTRGAENDERUNG  PIC X(10).
      ** Aenderungs-/ErstellungsDatum des Vertragssatzes.
              15  'XX'-SA1-ANREDE         PIC X(02).
              15  'XX'-SA1-NAME           PIC X(40).
              15  'XX'-SA1-TITEL1         PIC X(25).
              15  'XX'-SA1-ADELSTITEL     PIC X(20).
              15  'XX'-SA1-VORNAMEVN      PIC X(25).
              15  'XX'-SA1-LANDKNZ        PIC X(03).
              15  'XX'-SA1-PLZ            PIC X(06).
              15  'XX'-SA1-ORT            PIC X(34).
              15  'XX'-SA1-ORTZUSATZKNZ   PIC X(01).
              15  'XX'-SA1-STRASSE        PIC X(40).
              15  'XX'-SA1-BERUF-1        PIC 9(04).
      **      Feld SANGSTD aus 'XX'-DTA Satzbeschreibung vom 14.03.12
              15  'XX'-SA1-BERUF-2        PIC 9(09).
      **      Feld SBRFTES aus 'XX'-DTA Satzbeschreibung vom 14.03.12
              15  'XX'-SA1-LSV-J-N        PIC X(01).
      **     Vertrag nimmt am Lastschriftverfahren teil, Ja/Nein
      **     X = Vertrag nimmt teil. SPACE = Vertrag nimmt nicht teil.
      **
      ****    15  'XX'-SA1-KLSV           PIC X(01).
      ****   KlarschriftLeser
              15  'XX'-SA1-ZAHLUNGSWEISE  PIC X(01).
              15  'XX'-SA1-PERSONENNR-VN  PIC 9(04).
      **     IPER VN
              15  'XX'-SA1-WHRG-KNZ       PIC X(03).
              15  'XX'-SA1-COBA-KUNDENNR  PIC X(10).
      **     Coba-Kundennummer
              15  'XX'-SA1-ABVERM         PIC 9(09).
      **     Vermittler
              15  'XX'-SA1-GEBDAT-VN      PIC X(08).
      ** Geburtsdatum VN i.d.F. DDMMJJJJ
              15  'XX'-SA1-LOESCHKNZ      PIC 9(01).
      **     '9' fuer Loeschung sonst '0'
      ** Die Loeschung '9' wird nur im Deltabestand geliefert und
      ** abgearbeitet.
      ** Im Komplettbestand wird die Loeschung eines Vertrags festge-
      ** stellt, wenn der Vertrag nicht mehr im Aenderungsbestand mitge-
      ** liefert wurde oder das Ablaufdatum ('XX'-SA1-VTRGENDE) ist er-
      ** reicht bzw. wurde ueberschritten.
      **
      ** Personensatz C2892
      ** (neu: Nachname, Str., Hausnr., PLZ, Ort und Land)
      ** Personensatz C2892 wegen weiterer Personen.
      ** Kann pro Vertragssatz 1:n vorhanden sein.
           05 'XX'-SATZ2 REDEFINES 'XX'-SATZ0.
            10 'XX'-SA2-KEY              PIC X(42).
      *       15  'XX'-SA2-SAZDTA        PIC X(01).
      **     'C' Konstant
      **      15  'XX'-SA2-SAZART        PIC X(03).
      **     '289' Konstant
      **      15  'XX'-SA2-VTRGNR        PIC 9(09).
      **     Vertragsnummer
      **      15  'XX'-SA2-PERSNR        PIC 9(04).
      **     PersonenNummer
      **      15  'XX'-SA2-PERSNR-LFDNR  PIC 9(04).
      **     TV-Teil Byte 1-4; Person laufende Nr.
      **      15  'XX'-SA2-TTARNAM       PIC X(08).
      **     TarifName
      **      15  'XX'-SA2-SSAZFUG       PIC X(01).
      **     '2' Konstant
      **      15  'XX'-SA2-GESCHAEFTST   PIC X(03).
      **     'XX'-Geschäftsstelle
      **      15  'XX'-SA2-ITVT          PIC 9(09).
      **     TV-Teil
      ************************
            10 'XX'-SA2-PERSONENSATZ.
              15 'XX'-SA2-VORNAME        PIC X(25).
      ** Vorname aus Partner
              15 'XX'-SA2-GEBDAT         PIC X(10).
              15 'XX'-SA2-GESCHL         PIC X(01).
              15 'XX'-SA2-BERUF-1        PIC 9(04).
      ** Feld SANGSTD aus 'XX'-SA2-DTA Satzbeschreibung vom 14.03.12
              15 'XX'-SA2-BERUF-2        PIC 9(09).
      ** Feld SBRFTES aus 'XX'-SA2-DTA Satzbeschreibung vom 14.03.12
              15 'XX'-SA2-PERSBEG        PIC X(10).
      ** Datum VersicherungsBeginn für die Versicherte Person
              15 'XX'-SA2-PERSABL        PIC X(10).
      ** Datum VersicherungsEnde für die Versicherte Person
              15 'XX'-SA2-PERSAEND       PIC X(10).
      ** Laufdatum
              15 'XX'-SA2-DTNSPR         PIC 9(04).
      ** Datenschutzsperre
              15 'XX'-SA2-NAME           PIC X(40).
              15 'XX'-SA2-ANREDE         PIC X(01).
              15 'XX'-SA2-TITEL2         PIC X(25).
              15 'XX'-SA2-STRASSE        PIC X(25).
              15 'XX'-SA2-LANDKNZ        PIC X(03).
              15 'XX'-SA2-PLZ            PIC X(06).
              15 'XX'-SA2-ORT            PIC X(25).
250816        15 'XX'-SA2-VORVERSSCHL    PIC 9(05).
250816        15 'XX'-SA2-VORVERSDATUM   PIC X(10).
250816        15 'XX'-SA2-VORVERSKUENDKZ PIC X(01).
250816*       15 'XX'-SA2-RESERVE        PIC X(94).
250816        15 'XX'-SA2-RESERVE        PIC X(78).
      ****
      ** DVAG-DTA Tarifsatz / C2893
      ** Kann pro Personensatz 1:n vorhanden sein.
           05  'XX'-SATZ3 REDEFINES 'XX'-SATZ0.
            10  'XX'-SA3-KEY                  PIC X(42).
      *       15  'XX'-SA3-SAZDTA             PIC X(01).
      **     'C' Konstant
      *       15  'XX'-SA3-SAZART             PIC X(03).
      **     '289' Konstant
      *       15  'XX'-SA3-VTRGNR             PIC 9(09).
      **     Vertragsnummer
      *       15  'XX'-SA3-PERSNR             PIC 9(04).
      **     PersonenNummer
      *       15  'XX'-SA3-PERSNR-LFDNR       PIC 9(04).
      **     TV-Teil Byte 1-4; Person laufende Nr.
      *       15  'XX'-SA3-TTARNAM            PIC X(08).
      **     TarifName
      *       15  'XX'-SA3-SSAZFUG            PIC X(01).
      **     '3' Konstant
      *       15  'XX'-SA3-GESCHAEFTST        PIC X(03).
      **     'XX'-Geschäftsstelle
      *       15  'XX'-SA3-ITVT               PIC 9(09).
      **     TV-Teil
      *************************
            10  'XX'-SA3-TARIFSATZ.
              15 'XX'-SA3-ZUGANGVTRGSTAND     PIC X(10).
      ** Zugangsvertragsstand
              15 'XX'-SA3-ZUGANGSERSTELLDAT   PIC X(10).
      ** Erstelldatum des Zugangs
              15 'XX'-SA3-ABGANGSVTRGSTAND    PIC X(10).
      ** Abgangsvertragsstand
              15 'XX'-SA3-TARNAM              PIC X(08).
      ** Tarifname
              15 'XX'-SA3-TARFORM             PIC X(10).
      ** Tarifform
              15 'XX'-SA3-VLEIST              PIC S9(9)V99.
      ** Versicherungsleistung
              15 'XX'-SA3-SB-GESAMT           PIC S9(9)V99.
      ** Selbstbeteiligung GESAMT. Summe aus
      **                           BVSGSBG_IND,
      **                           BVSGSBG_TAR
      **                       und BVSGSBG_TAF (Bezeichnungen der 'XX')
              15 'XX'-SA3-BEITRAG             PIC S9(9)V99.
      ** Tarifbeitrag
120218        15 'XX'-SA3-STVTBTG             PIC X(02).
      ** Kennzeichen Anwartschaft
              15 'XX'-SA3-BE-REDUK-BTG        PIC S9(9)V99.
      ** Beitragsreduktion aus BE-Tarif
              15 'XX'-SA3-RESERVE             PIC X(206).
              15 'XX'-SA3-ANZ-TAR-DUP         PIC 99.
      **
      ** Banksatz C2894 (komplett neu)
      ** DVAG-DTA BankSatz / C2894
      ** Wird dem VN (Satzart 1) zugerechnet und darf daher, wenn vor-
      ** handen nur einmal pro Vertragsnummer vorhanden sein.
      ** Banksatz kann, muss jedoch nicht vorhanden sein.
           05  'XX'-SATZ4 REDEFINES 'XX'-SATZ0.
            10  'XX'-SA4-KEY                  PIC X(42).
      *       15  'XX'-SA4-SAZDTA             PIC X(01).
      **     'C' Konstant
      *       15  'XX'-SA4-SAZART             PIC X(03).
      **     '289' Konstant
      *       15  'XX'-SA4-VTRGNR             PIC 9(09).
      **     Vertragsnummer
      *       15  'XX'-SA4-PERSNR             PIC 9(04).
      **     PersonenNummer
      *       15  'XX'-SA4-PERSNR-LFDNR       PIC 9(04).
      **     TV-Teil Byte 1-4; Person laufende Nr.
      *       15  'XX'-SA4-TTARNAM            PIC X(08).
      **     TarifName
      *       15  'XX'-SA4-SSAZFUG            PIC X(01).
      **     '1' Konstant
      *       15  'XX'-SA4-GESCHAEFTST        PIC X(03).
      **     'XX'-Geschäftsstelle
      *       15  'XX'-SA4-ITVT               PIC 9(09).
      **     TV-Teil
      *************************************
            10  'XX'-SA4-BANKSATZ.
              15 'XX'-SA4-LFD-BANK-NR         PIC 9(05).
      ** laufende Banknummer
              15 'XX'-SA4-BANKTYP             PIC X(01).
      ** Bank-TYP
              15 'XX'-SA4-BANKNAME            PIC X(30).
      ** Bankname
     ********  15 'XX'-SA4-KONTOINHABER        PIC X(40).
     **** Kontoinhaber
               15 'XX'-SA4-NAMEBZ             PIC X(40).
     **** NAME abweichender Beitragszahler
              15 'XX'-SA4-IBAN                PIC X(34).
      ** IBAN / Kontonummer
      ** Die Kontonummer wird in seinem zukuenftigen Nachfolgerfeld
      ** IBAN geliefert. Mail von Herrn Chalupka(CENTRAL) vom
      ** 03.05.2012 um 14.11 Uhr.
              15 'XX'-SA4-BIC                 PIC X(11).
      ** Bankinterne Clearing Nummer
              15 'XX'-SA4-BLZ                 PIC X(08).
      ** BankLeitZahl -entfaellt in absehbarer Zeit-
150613******** neu für sepa
               15 'XX'-SA4-VORNAMEBZ              PIC X(25).
     **** Vorname abweichender Beitragszahler
               15 'XX'-SA4-STRASSEBZ              PIC X(25).
     **** Strasse abweichender Beitragszahler
               15 'XX'-SA4-LANDBZ                 PIC X(03).
     **** Landkz abweichender Beitragszahler
               15 'XX'-SA4-PLZBZ                  PIC X(06).
     **** PLZ abweichender Beitragszahler
               15 'XX'-SA4-ORTBZ                  PIC X(25).
     **** Ort abweichender Beitragszahler
               15 'XX'-SA4-GEBDATBZ               PIC X(10).
     **** Geburtsdatum abweichender Beitragszahler
               15 'XX'-SA4-ANREDEBZ               PIC X(02).
     **** Anrede abweichender Beitragszahler
    *****       15 'XX'-SA4-RESERVE             PIC X(173).
150613        15 'XX'-SA4-RESERVE             PIC X(77).
      **
 
      ** Satz-DateiEnde
           05  'XX'-SATZE REDEFINES 'XX'-SATZ0.
            10  'XX'-SAE-ENDESATZ.
              15 'XX'-SAE-KNZ         PIC X(01).
      ** 'E' Konstant
              15 'XX'-SAE-GESAMT      PIC 9(07).
      ** Anzahl gelieferte Datensätze ohne Anfang- und Endesatz.
              15 'XX'-SAE-RESERVE     PIC X(336).
      ******************************************************************
 
 
 



