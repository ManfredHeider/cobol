      *---------------------------------------------------------------*
      *    STANDARD FUER ALLE OBJEKTE UNTER PVCS VERSIONMANAGER
      *            DATUM      :  24.04.2018
      *            KATEGORIE  :  COPYSTRECKE
      *---------------------------------------------------------------*
      *    COPYRIGHT (C) 2018 ATLAS DIENSTLEISTUNGEN F. V. GMBH
      *---------------------------------------------------------------*
      *    $WORKFILE:   DRPA1890.CPY  $
      *    $REVISION:   2.1  $
      *
      *    $LOG:   L:/MF/CPY/DRPA1890.CPV  $
      *
      *   REV 2.1   MAY 22 2018 07:43:24   T13EX67
      *APA02579: FRIWI: PROVISIONSVERTEILUNG- UND ABRECHNUNG - MSS
      *
      *   REV 2.0   APR 24 2018 15:10:58   T13EX67
      *ARCHIVERSTELLUNG
      *
      *---------------------------------------------------------------*

      ****************************************************************
      *    COPY-MEMBER: DRPA1890 ERWEITERUNG NPM-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    24.04.2018                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01579!24.04.18!02.1! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG XG09-PG17 VERWENDET            *
      *    ES ENTHÄLT ERWEITERUNGEN FÜR DIE FELDER DES               *
      *    NPM-DATEN-WORDKFLOW.                                      *
      *    - EINIGE BEZEICHNUNGEN UND TEXTE                          *
      *    - FÜR EINGABEFELDER (INTENSIV, OFFEN UND AENDERUNG-KZ)    *
      *    - FÜR AUSGABEFELDER (INTENSIV)                            *
      ****************************************************************
       01 NPM-DATEN-NEU-ERW.
          05 PM-DATEN.
      ***    GESELLSCHAFT
             10 GESELLSCHAFT-BEZ10                PIC  X(010).
             10 PGEB-GES.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
             10 VERTRIEBS-GES-BEZKURZ             PIC  X(004).
      ***    EXT. VERTRAGSNUMMER
             10 VNR.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    UNTERNUMMER
             10 UNR.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    SCHICHTNUMMMER
             10 SCHICHTNR.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    WÄHRUNG
             10 WAEHRUNG-BEZ-KURZ                 PIC  X(004).
             10 WAEHRUNG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    KUNDENNAME
             10 KUNDE-NAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    FAELLIGKEIT
             10 FAELLIGKEIT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    KUNDEN-VORNAME
             10 KUNDE-VNAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    ABSCHLUSSVERMITTLER + PRÜFZIFFER
             10 AVERM.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
             10 AVERMPZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    ABRECHNUNGSDATUM PRODUKTGEBER
             10 ABR-DAT-PG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    ZAHLWEISE
             10 ZAHLWEISE-TEXT                    PIC  X(010).
             10 ZAHLWEISE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    VERTRAG-BEGINN
             10 VERTRAG-BEGINN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VERTRAG-ENDE
             10 VERTRAG-ENDE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    HAFTZEIT BEGINN
             10 HAFTBEGINN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    HAFTZEIT ENDE
             10 HAFTENDE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VERSICHERTE PERSON NAME
             10 VP-NAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VERSICHERTE PERSON VORNAME
             10 VP-VORNAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    INKASSO-STAND DATUM
             10 INKASSO-DATUM.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    DYNAMIK-KZ-2
             10 DYNAMIK-KZ-2.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    DYNAMIK-KZ-BEZEICHNUNG
             10 DYNKZ-BEZ-KURZ                    PIC  X(001).
             10 DYNKZ-BEZ10                       PIC  X(010).
      ***    LAUFZEIT
             10 LAUFZEIT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    RABATTIERUNGSSTUFE
             10 RAB-STUFE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    RABATTIERUNGSSTUFE BEZEICHNUNG
             10 RAB-STUFE-KURZ                    PIC  X(004).
      ***    POLICIERUNGSDATUM
             10 POL-DAT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    KENNUNG
             10 KENNUNG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    PG-PRODUKTBEZEICHNUNG
             10 PG-PROD-BEZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      *
      *** PRODUKT-DATEN.
          05 PRODUKT-DATEN.
      ***       NORMIERTER PRODUKTGEBER-GESCHÄFTSVORFALL
                15 NPGV.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       BEWEGUNGSHERKUNFT
                15 BEW-HERKUNFT.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PG-GEVO
                15 PG-GEVO.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PRODUKT
                15 PRODUKT.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PG-TARIF
                15 PG-TARIF.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PROVISIONSPFLICHTIGE SUMME
                15 PPFL-SUMME.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PROVISIONSSATZ
                15 PROV-SATZ.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PROVISIONSBETRAG
                15 PROV-BETRAG.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************

