      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.09.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2682.CPY  $
      *    $Revision:   2.7  $
      *
      *    $Log:   L:/mf/CPY/DRKI2682.CPv  $
      *
      *   Rev 2.7   Oct 22 2018 08:21:22   t13ae31
      *AKI01630: CKV Bordero neues Feld Programme
      *
      *   Rev 2.6   Oct 02 2017 11:05:10   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.5   Sep 14 2017 14:02:44   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.4   Sep 06 2017 14:15:38   t13ex78
      *AKI01544: Programme & Copies
      *
      *   Rev 2.3   Mar 27 2017 09:30:14   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   Aug 08 2013 09:26:26   t13ex52
      *AKI01226: G50 CENTRAL BAP Tarifumstellung 2013
      *
      *   Rev 2.1   Oct 18 2012 10:15:00   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Sep 18 2012 19:50:46   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FUER BAP-Datensaetze(BeitragsAnpassung) Central *
      *    CKV Zwischenbereich zur Pruefung der DTA-Lieferung.         *
      *    ----------------------------------------------------------- *
      *    SATZLAENGE 100                                              *
      *                                                                *
      *    ANGELEGT 18.09.2012     / Schlenther                        *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                         ! Name          *
      *AUFTRAG !                                       !               *
      *--------!---------------------------------------!---------------*
      *18.09.12! Neuerstellung                         ! Schlenther    *
      *AKI01147!                                       !               *
      *--------!---------------------------------------!---------------*
180713*18.07.13! Integration zusätzliches Feld         ! Schlenther    *
    >>*AKI01226! "Selbstbehalt" in die Satzart D400    !               *
180713*--------!---------------------------------------!---------------*
      *24.03.17! FVD entfernt                          ! HHU           *
      *FKI01544!                                       !               *
050917*--------!---------------------------------------!---------------*
      *05.09.17! Neues Feld LEISTUNG in SA 400         ! tfl           *
      *AKI01544!                                       !               *
      *--------!---------------------------------------!---------------*
110918*11.09.18! Neues Feld Beitragsvorteil SA200      ! HHU           *
      *AKI01630!                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      *        !                                       !               *
      ******************************************************************
       01  DRKI2682.
      ** DTA VORSATZ / A-Satz zur Identifikation des DTZA-Inhaltes
      ** Darf nur 1 x je DTA vorhanden sein.
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
             10  CKV-SAA-EMPFAENGER2     PIC X(22).
      ** 'FFM-RECHENZENTRUM-5.OG'
             10  CKV-SAA-BESTANDSART     PIC X(08).
      ** 'ZUKBEITR' Konstanter Inhalt.
             10  FILLER                  PIC X(22).
      **
      ** Vertragssatz 'D100'
      ** Kann nur 1 x je VertragsNr. vorhanden sein.
           05  CKV-SATZ1 REDEFINES CKV-SATZ0.
            10  CKV-SA1-KEY.
      ** 'D' Konstant
              15  CKV-SA1-SATZART         PIC X(01).
      ** '100' Konstant
              15  CKV-SA1-DATENSATZART    PIC X(03).
      ** Vertragsnummer
              15  CKV-SA1-VTRGNR          PIC 9(09).
      **
            10 CKV-SA1-VERTRAGSSSATZ.
      ** Erhoehung in % Vertrag
              15 CKV-SA1-ERHOEHUNG-PROZ  PIC S9(07)V99.
      ** Beitragsrueckerstattung Vertrag
              15 CKV-SA1-BEITRAG-RUECK   PIC S9(07)V99.
      ** Kundenbindung Vertrag (nur J/N erlaubt)
              15 CKV-SA1-KUNDENBINDUNG   PIC X(01).
      ** In Stornohaftung (nur J/N erlaubt)
              15 CKV-SA1-IN-STORNOHAFTG  PIC X(01).
      **
              15 FILLER                  PIC X(67).
      **
      ** Personensatz 'D200'
      ** Kann pro Vertragssatz 1:n vorhanden sein. Es gibt je Person
      ** jedoch nur einen Personensatz.
           05  CKV-SATZ2 REDEFINES CKV-SATZ0.
            10  CKV-SA2-KEY.
              15  CKV-SA2-SATZART         PIC X(01).
      ** 'D' Konstant
              15  CKV-SA2-DATENSATZART    PIC X(03).
      ** '200' Konstant
              15  CKV-SA2-VTRGNR          PIC 9(09).
      ** Vertragsnummer
      **
            10 CKV-SA2-PERSONENSATZ.
      ** PersonenNummer der versicherten PERSON(VP)
              15 CKV-SA2-VPNR              PIC 9(04).
      ** Erhoehung in % PERSON
              15 CKV-SA2-ERHOEHUNG-PROZ    PIC S9(07)V99.
      ** Alterungsrueckstellungen PERSON
              15 CKV-SA2-ALTRUECKSTELLG    PIC S9(07)V99.
      ** Beitragsrueckerstattung PERSON
              15 CKV-SA2-BEITRAGERSTATTUNG PIC S9(07)V99.
      ** Kundenbindung PERSON (nur J/N erlaubt)
              15 CKV-SA2-KUNDENBINDUNG     PIC X(01).
      ** Beitragsvorteil einer PERSON
110918        15 CKV-SA2-BEITRAGSVORTEIL   PIC S9(07)V99.
      ** Leerstellen
      **      15 FILLER                    PIC X(55).
110918        15 FILLER                    PIC X(46).
      ****
      ** DVAG-DTA Historie 'D300'
      ** Pro Jahr (max. 4 Jahre), Person, Tarif ein Datensatz.
           05  CKV-SATZ3 REDEFINES CKV-SATZ0.
            10 CKV-SA3-KEY.
              15  CKV-SA3-SATZART         PIC X(01).
      ** 'D' Konstant
              15  CKV-SA3-DATENSATZART    PIC X(03).
      ** '300' Konstant
              15  CKV-SA3-VTRGNR          PIC 9(09).
      ** Vertragsnummer
      **
            10  CKV-SA3-HISTORIESATZ.
      ** Nummer der versicherten PERSON(VP)
              15 CKV-SA3-VPNR              PIC 9(04).
      ** Datum im Format JJJJMM
              15 CKV-SA3-DATUM             PIC 9(06).
      ** Tarif (z. Bsp. V3332S2)
              15 CKV-SA3-TARIF             PIC X(12).
      ** Tagessatz
              15 CKV-SA3-TAGESSATZ         PIC S9(07)V99.
      ** Beitrag zum Datum
              15 CKV-SA3-BEITRAG-ZUM-DATUM PIC S9(07)V99.
      ** Gesetzlicher Zuschlag
              15 CKV-SA3-GESETZL-ZUSCHLAG  PIC S9(07)V99.
      ** Risikozuschlag
              15 CKV-SA3-RISIKOZUSCHLAG    PIC S9(07)V99.
      ** Selbstbehalt fuer den Vertrag
              15 CKV-SA3-SELBSTBEHALT      PIC S9(07)V99.
      ** Zeitlich befristete Gutschrift
              15 CKV-SA3-ZEITL-BEFRIST-GUTSCHR PIC S9(07)V99.
      ** Leerstellen
              15 FILLER                    PIC X(11).
      **
      ** Umstellungstarife 'D400'
      ** Pro Person und Tarif ein Umstellungstarif
      *  Tabelle CKVPERSUMSTTARIF(TCKI0453)
      *          KRANKEN_VORSCHLAG_PERS_UMSTELL
           05  CKV-SATZ4 REDEFINES CKV-SATZ0.
            10  CKV-SA4-KEY.
              15  CKV-SA4-SATZART         PIC X(01).
      ** 'D' Konstant
              15  CKV-SA4-DATENSATZART    PIC X(03).
      ** '400' Konstant
              15  CKV-SA4-VTRGNR          PIC 9(09).
      ** Vertragsnummer
      **
            10 CKV-SA4-UMSTELLUNGSTARIFSATZ.
      ** PersonenNummer der versicherten PERSON(VP)
              15 CKV-SA4-VPNR              PIC 9(04).
      ** Tarif (z. Bsp. V3332S2)
              15 CKV-SA4-TARIF             PIC X(12).
      ** Beitrag
              15 CKV-SA4-BEITRAG           PIC S9(07)V99.
      ** Risikozuschlag
              15 CKV-SA4-RISIKOZUSCHLAG    PIC S9(07)V99.
      ** Zeitlich befristete Gutschrift (Neu ab 2013-01)
              15 CKV-SA4-ZEITL-BEFRIST-GUTSCHR PIC S9(07)V99.
180713** Selbstbehalt (Neu ab 2014-01)
              15 CKV-SA4-SELBSTBEHALT      PIC S9(07)V99.
050917** Leistung
              15 CKV-SA4-LEISTUNG          PIC S9(07)V99.
      ** Leerstellen
050917        15 FILLER                    PIC X(26).
      **
      ******************************************************************
 
 
 
 

