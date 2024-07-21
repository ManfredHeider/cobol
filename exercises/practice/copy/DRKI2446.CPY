      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.03.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2446.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2446.CPv  $
      *
      *   Rev 2.1   Mar 29 2011 11:55:32   t13ex15
      *AKI00994: AllianzGI: PGM Bereinigung von DAGI-Bestandsdepots
      *
      *   Rev 2.0   Mar 29 2011 11:45:50   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *----------------------------------------------------------------*
      *                                                                *
      *    COPY-MEMBER FÜR BESTANDSÄNDERUNG DIT (DTA)                  *
      *    ----------------------------------------------------------  *
      *                                                                *
      *    DATEINAME = DTADITB                                         *
      *                                                                *
141101* Neu in 2001:                                                   *
141101*    SATZLÄNGE   200                                             *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                          !NAME     !VER-*
      *AUFTRAG !                                        !         !SION*
      *--------!----------------------------------------!--------_!----*
      *29.03.11!NEUERSTELLUNG F. ORIGINAL DTA Bestand   !SWZ      ! 01 *
      *KI-NEU  !(AGI)                                   !         !    *
      *--------!----------------------------------------!--------_!----*
      *        !                                        !         !    *
      *--------!----------------------------------------!---------!----*
      *--------!----------------------------------------!---------!----*
      *
CCC009*    SKIP1
       01  DRKI0002.
           10  DIT-VORSATZ.
      *
               20  DIT-V-SA            PIC X(001).
      *            SATZART = 'A'
               20  DIT-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DIT,HOF'
               20  DIT-V-DATUM         PIC 9(008).
      *            BUCHUNGSDATUM = 'JJJJMMTT'
               20  DIT-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               20  DIT-V-TEXT          PIC X(010).
      *            INHALTSANGABE:
141101*            Tägliche Lieferung normale Fonds     'BUCHDE'
141101*            Tägliche Lieferung VL                'BUCHVL'
141101*            Lieferung Ausschüttung normale Fonds 'BUCHDEAUS'
141101*            Lieferung Ausschüttung VL            'BUCHVLAUS'
141101*            Komplettbestand normale Fonds        'BUCHDEKPž
141101*            Komplettbestand VL                   'BUCHVLKPž

               20  DIT-V-INHALT        PIC X(001).
      *            DATENSÄTZE ÜBERTRAGEN: JA = SPACE   NEIN = 'K'

220301*        20  FILLER              PIC X(098).

220301         20  DIT-V-WKZ           PIC X(003).
      *            Währungs-Kennz. =  'EUR'
141101*        20  FILLER              PIC X(095).
141101         20  FILLER              PIC X(165).
      *            RESERVE

      *
           10  DIT-SATZ     REDEFINES      DIT-VORSATZ.
      *
               20  DIT-SA                  PIC  X(001).
      *            SATZART        = 'C'
               20  DIT-GES                 PIC  9(002).
      *            GESELLSCHAFT   = '40'
160109         20  DIT-KEY.
>>                 25  DIT-VNR                 PIC  9(015).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
                   25 FILLER REDEFINES DIT-VNR.
                      30  DIT-VNR-1-13         PIC  9(013).
                      30  DIT-UNTER-NR         PIC  9(002).
      *            UNTERDEPOTNUMMER)
                   25  DIT-WP-KENN-NR          PIC  X(06).
      *            WERTPAPIERKENN-NUMMER
<<    *            25  DIT-KTO-ART             PIC  X(02).
160109*            Kontoart (DE, VL)
               20  DIT-ANTEILE             PIC  9(06)V999.
      *            ANTEILE (BESTAND STÜCK)
               20  DIT-BUCH-DAT            PIC  9(08).
      *            BUCHUNGSDATUM
               20  DIT-BUCH-BETRAG         PIC  9(07)V99.
      *            BETRAG LETZTE BUCHUNG
               20  DIT-BUCH-ST-BETRAG      PIC  9(07)V99.
      *            STEUERBETRAG LETZTE BUCHUNG
               20  DIT-BUCH-SCHL           PIC  X(02).
      *            BUCHUNGSSCHLÜSSEL
               20  DIT-EINZ-KUM            PIC  9(13).
      *            KUMULIERTE EINZAHLUNGEN
               20  DIT-AUSZ-KUM            PIC  9(13).
      *            KUMULIERTE AUSZAHLUNGEN
141101*        20  DIT-WERT-GES            PIC  9(03)V99.
      *            WERTENTWICKLUNG GESAMT
141101*        20  DIT-WERT-JAHR           PIC  9(03)V99.
      *            WERTENTWICKLUNG LFD. JAHR
141101*        20  DIT-FREI-VERBR          PIC  9(05)V99.
      *            VERBRAUCHTER FREISTELLUNGSBETRAG
               20  DIT-ZINS-AB-ST          PIC  9(06)V99.
      *            ZINSABSCHLAGSTEUER
190499*        20  FILLER                  PIC  X(18).
<<             20  DIT-KTO-ART             PIC  X(02).
160109*            Kontoart (DE, VL)
230100         20  DIT-FREI-VERBR-EURO     PIC  9(05)V99.
      *            Verbr. FREISTELLUNGSBETRAG IN EURO

141101* Neue Felder ab 2002
141101         20  DIT-ZW-GEWINN           PIC S9(05)V99.
...   *            ZWISCHENGEWINN GUTHABEN
...            20  DIT-BUCH-ANTEILE        PIC  9(06)V999.
...   *            BUCHUNGSBESTAND STÜCKZAHL
...            20  DIT-UMSATZ-NETTO        PIC  9(07)V99.
...   *            NETTOBETRAG = RÜCKNAHMEPREIS X ANTEILE
141107         20  DIT-GEB-BETRAG          PIC  9(07)V99.
..!   *            GEBÜHRENBETRAG
..!            20  DIT-GEB-SCHLUESSEL      PIC  9(02).
..!   *            GEBÜHRENSCHLÜSSEL
..!            20  DIT-GEB-TEXT            PIC  X(20).
..!   *            GEBÜHRENTEXT
141107         20  FILLER                  PIC  X(40).

190499*        20  FILLER                  PIC  X(16).
141101*        20  FILLER                  PIC  X(09).
      *            RESERVE
141101*        20  FILLER                  PIC  X(71)
...   *            RESERVE

      *
           10  DIT-NACHSATZ    REDEFINES     DIT-VORSATZ.
      *
               20  DIT-N-SA            PIC X(001).
      *            SATZART             'E'
               20  DIT-N-ANZ           PIC 9(009).
      *            ANZAHL SAETZE INCL. VOR- UND NACHSATZ
141101*        20  FILLER              PIC X(120).
141101         20  FILLER              PIC X(190).
      *            RESERVE

