      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2013
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2013 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1737.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1737.CPv  $
      *
      *   Rev 2.2   Mar 26 2014 09:39:40   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): Copy's Dialoge
      *
      *   Rev 2.1   Dec 17 2013 11:16:56   t13ex67
      *APA01526: Projekt Kalypso Teilprojekt 5 (Provisionsverteilun
      *g): PEG
      *
      *   Rev 2.0   Mar 04 2013 10:49:58   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1737 ERWEITERUNG NPM-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    04.03.2013                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01526!04.03.13!02.1! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG XG06-PG17 VERWENDET            *
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
      ***    ABSCHLUßVERMITTLER + PRÜFZIFFER
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
      ***    NEUSTORNO-KZ
             10 NEUSTORNO-KZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    DYNAMIK-KZ-2
             10 DYNAMIK-KZ-2.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    DYNAMIK-KZ-2-BEZEICHNUNG
             10 DYNKZ2-BEZ-KURZ                   PIC  X(001).
             10 DYNKZ2-BEZ10                      PIC  X(010).
      ***    VERMITTELNDE GESELLSCHAFT
             10 VERMGES.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    PRODART
             10 PRODART.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VVG-KZ
             10 VVG-KZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    LAUFZEIT
             10 LAUFZEIT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    TARIF-KZ
             10 TARIF-KZ.
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
      ***    PROVISIONSSCHLÜSSEL
             10 PROV-SCHLUESSEL.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    URSPRUNGSPOLICIERUNGSDATUM
             10 URSPR-POL-DAT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    POLICIERUNGSDATUM
             10 POL-DAT.
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
      ***       BEWEGUNGSSCHLÜSSEL
                15 BEW-SCHLUESSEL.
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
