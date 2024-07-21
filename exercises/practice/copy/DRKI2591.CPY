      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  11.04.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2591.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2591.CPv  $
      *
      *   Rev 2.3   Mar 27 2017 09:29:36   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   Apr 23 2012 12:37:00   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.1   Apr 13 2012 11:32:56   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Apr 11 2012 17:55:08   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BESTANDSAETZE CKV direkte Datenlieferung   *
      *    vor der Sortierung in der aktuellen Form (11-04-2012)       *
      *    -----------------------------------                         *
      *    SATZLAENGE 340                                              *
      *                                                                *
      *    ANGELEGT 11.04.2012     / Schlenther                        *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         ! Name          *
      *AUFTRAG !                                       !               *
      *--------!---------------------------------------!---------------*
      *11.04.12! Neuerstellung wegen neuer             ! Schlenther    *
      *AKI01100! Schnittstelle.                        !               *
      *--------!---------------------------------------!---------------*
      *24.03,17! FVD entfernt                          ! HHU           *
      *FKI01544!                                       !               *
      *--------!---------------------------------------!---------------*
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      ******************************************************************
       01  DRKI2591.
      ** DTA VORSATZ / A-Satz wegen Kennzeichnung Gesamt- bzw.
      ** Aenderungsbestand. Kann nur 1 x je DTA vorhanden sein.
           05 CKV-SATZA.
             10  CKV-SAA                 PIC X(01).
      ** 'A' Constant
             10  CKV-SAA-ABSENDER        PIC X(27).
      ** 'CENTRAL KRANKENVERS. KOELN '
             10  CKV-SAA-ERSTELLDATUM    PIC 9(08).
      ** ErstellDatum in der Form TT.MM.JJJJ
             10  CKV-SAA-DTASTAND        PIC 9(08).
      ** Stichtag in der Form TT.MM.JJJJ
             10  CKV-SAA-EMPFAENGER1     PIC X(04).
      ** 'DVAG'
      ** 'ALLF'
             10  CKV-SAA-EMPFAENGER2     PIC X(23).
      ** '/FFM-RECHENZENTRUM-5.OG'
             10  CKV-SAA-DTA-VTRG        PIC X(17).
      ** 'DTA-VERTRAGSDATEN' Konstanter Inhalt.
             10  CKV-SAA-BESTAND-ART     PIC X(01).
      ** '1' Kennzeichen Komplettbestand
             10  FILLER                  PIC X(02).
             10  CKV-SAA-WHRG-KNZ        PIC X(03).
             10  CKV-SAA-RESERVE0        PIC X(246).
      **
      ** Vertragssatz C2891 wegen Loeschkennzeichen - noetig nach Um-
      ** stellung auf Deltalieferung.
      ** Kann nur 1 x je VertragsNr. vorhanden sein.
           05 CKV-SATZ1 REDEFINES CKV-SATZA.
      ** ==============> Beginn KEY <================================
              10  CKV-SA1-KEY.
                15  CKV-SA1-SAZDTA          PIC X(01).
      ** 'C' Konstant
                15  CKV-SA1-SAZART          PIC X(03).
      ** '289' Konstant
                15  CKV-SA1-SSAZFUG         PIC X(01).
      ** '1' Konstant
                15  CKV-SA1-GST             PIC X(03).
      ** CKV-SA1-Geschäftsstelle
                15  CKV-SA1-VTGNR           PIC 9(09).
      ** Vertragsnummer
                15  CKV-SA1-IPERNR          PIC 9(04).
      ** Nummer der versicherten Person
      ** ==============> Ende KEY <===================================
              10  CKV-VERTRAGSSATZ.
                15  CKV-SA1-ITVT            PIC 9(09).
      ** TV-Teil
                15  CKV-SA1-TTARNAM         PIC X(08).
      ** TarifName
                15  CKV-SA1-SONDBETR        PIC X(11).
                15  CKV-SA1-PARTINF         PIC X(15).
                15  CKV-SA1-VTGBEGINN       PIC X(10).
                15  CKV-SA1-VTGENDE         PIC X(10).
                15  CKV-SA1-VTGERSTELLUNG   PIC X(10).
                15  CKV-SA1-ANREDE          PIC X(02).
                15  CKV-SA1-NAME            PIC X(40).
                15  CKV-SA1-TITEL           PIC X(25).
                15  CKV-SA1-ADELSTITEL      PIC X(20).
                15  CKV-SA1-VORNAMEVN       PIC X(25).
                15  CKV-SA1-LANDKNZ         PIC X(03).
                15  CKV-SA1-PLZ             PIC X(06).
                15  CKV-SA1-ORT             PIC X(34).
                15  CKV-SA1-ORTZUSATZKNZ    PIC X(01).
                15  CKV-SA1-STRASSE         PIC X(40).
                15  CKV-SA1-BERUF-1         PIC 9(04).
      ** Feld SANGSTD aus CKV-DTA Satzbeschreibung vom 12.04.12
                15  CKV-SA1-BERUF-2         PIC 9(09).
      ** Feld SBRFTES aus CKV-DTA Satzbeschreibung vom 12.04.12
                15  CKV-SA1-KLSV            PIC X(01).
      ** KlarschriftLeser
                15  CKV-SA1-ZAHLUNGSWEISE   PIC X(01).
                15  CKV-SA1-PNRVN           PIC 9(04).
      ** IPER VN
                15  CKV-SA1-WHRG-KNZ        PIC X(03).
      ** Währungskennzeichen i.d.R. EUR
                15  CKV-SA1-COBA-KUNDENNR   PIC X(10).
      ** Coba-Kundennummer
                15  CKV-SA1-ABVERM          PIC 9(09).
      ** Vermittler
                15  FILLER                  PIC X(09).
      **
      ** Personensatz C2892 wegen weiterer Personen.
      ** Kann pro Vertragssatz 1:N vorhanden sein.
           05 CKV-SATZ2 REDEFINES CKV-SATZA.
      ** ==============> Beginn KEY <================================
             10 CKV-SA2-KEY.
               15 CKV-SA2-SAZDTA          PIC X(01).
      ** 'C' Konstant
               15 CKV-SA2-SAZART          PIC X(03).
      ** '289' Konstant
               15 CKV-SA2-SSAZFUG         PIC X(01).
      ** '2' Konstant
               15 CKV-SA2-GST             PIC X(03).
      ** CKV-Geschäftsstelle
               15 CKV-SA2-VTGNR           PIC 9(09).
      ** Vertragsnummer
               15 CKV-SA2-IPERNR          PIC 9(04).
      ** Nummer der versicherten Person
               15 CKV-SA2-ITVT            PIC 9(09).
      ** TarifTeil
               15 CKV-SA2-TTARNAM         PIC X(08).
      ** TarifName
      ** ==============> Ende KEY <================================
      *
             10 CKV-PERSONENSATZ.
               15 CKV-SA2-VORNAME         PIC X(25).
      ** Vorname aus Partner
               15 CKV-SA2-GEBDAT          PIC X(10).
      ** GeburtsDatum i.d.F. DD.MM.JJJJ
               15 CKV-SA2-GESCHL          PIC X(01).
      ** Geschlecht
               15 CKV-SA2-BERUF-1         PIC 9(04).
      ** Feld SANGSTD aus CKV-DTA Satzbeschreibung vom 12.04.12
               15 CKV-SA2-BERUF-2         PIC 9(09).
      ** Feld S0RFTES aus CKV-DTA Satzbeschreibung vom 12.04.12
               15 CKV-SA2-PERSBEG         PIC X(10).
      ** Datum VersicherungsBeginn für die VP(Versicherte Person)
      ** i.d.F. DD.MM.JJJJ
               15 CKV-SA2-PERSABL         PIC X(10).
      ** Datum VersicherungsEnde für die VP i.d.F. DD.MM.JJJJ
               15 CKV-SA2-PERSAEND        PIC X(10).
      ** Laufdatum i.d.F. DD.MM.JJJJ
               15 CKV-SA2-DTNSPR          PIC 9(04).
      ** Datenschutzsperre
               15 CKV-SA2-RESERVE         PIC X(219).
      ****
      ** DVAG-DTA Tarifsatz / C2893
      ** Kann pro Personensatz 1:N vorhanden sein.
           05 CKV-SATZ3 REDEFINES CKV-SATZA.
      ** ==============> Beginn KEY <================================
             10 CKV-SATZ3-KEY.
               15 CKV-SA3-SAZDTA              PIC X(01).
      ** 'C' Konstant
               15 CKV-SA3-SAZART              PIC X(03).
      ** '289' Konstant
               15 CKV-SA3-SSAZFUG             PIC X(01).
      ** '3' Konstant
               15 CKV-SA3-GST                 PIC X(03).
      ** CKV-Geschäftsstelle
               15 CKV-SA3-VTGNR               PIC 9(09).
      ** Vertragsnummer
               15 CKV-SA3-IPERNR              PIC 9(04).
      ** Nummer der versicherten Person
               15 CKV-SA3-ITVT                PIC 9(09).
      ** TarifTeil
               15 CKV-SA3-TTARNAM             PIC X(08).
      ** TarifName
      ** ==============> Beginn KEY <================================
             10 CKV-TARIFDATEN.
               15 CKV-SA3-ZUGANGSVTRGSTAND     PIC X(10).
      ** Zugangsvertragsstand Datum i.d.F. DD.MM.JJJJ
               15 CKV-SA3-ZUGANGSERSTELLDAT    PIC X(10).
      ** Erstelldatum des Zugangs i.d.F. DD.MM.JJJJ
               15 CKV-SA3-ABGANGSVTRGSTAND     PIC X(10).
      ** Abgangsvertragsstand Datum i.d.F. DD.MM.JJJJ
               15 CKV-SA3-TARNAM               PIC X(08).
      ** Tarifname
               15 CKV-SA3-TARFORM              PIC X(10).
      ** Tarifform
               15 CKV-SA3-VLEIST               PIC S9(9)V99.
      ** Versicherungsleistung
               15 CKV-SA3-SBTEIL               PIC S9(9)V99.
      ** Selbstbeteiligung GESAMT Summe aus BVSGSBG_IND, BVSGSBG_TAR
      ** und BVSGSBG_TAF (Bezeichnungen der CKV)
               15 CKV-SA3-BEITRAG              PIC S9(9)V99.
      ** Tarifbeitrag
               15 CKV-SA3-RESERVE              PIC X(221).
      **
      ** Satz-DateiEnde
           05  CKV-SATZE REDEFINES CKV-SATZA.
             10 CKV-SAE              PIC X(01).
             10 CKV-GESAMT-ZEILEN    PIC 9(07) COMP-3.
             10 CKV-RESERVEE         PIC X(335).
 
      ******************************************************************

