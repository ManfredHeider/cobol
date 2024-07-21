      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1804.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRPA1804.CPv  $
      *
      *   Rev 2.2   May 18 2017 15:37:10   t13ex67
      *APA02360: Fingerhaus - PG17 MSS
      *
      *   Rev 2.1   Jul 16 2015 14:57:24   t13ex67
      *APA01932: USt.pfl. Provisionen / DAISY und VB-System: PG17
      *
      *   Rev 2.0   Mar 17 2015 09:31:10   t13ex67
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1804 ERWEITERUNG NPM-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH-SCHÄFER                                  *
      *    DATUM:    17.03.2015                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA01932!17.03.15!02.1! NEUERSTELLUNG             ! STORCH   *
      * APA02360!04.05.17!02.2! ERWEITERUNGEN FÜR         ! STORCH   *
      *         !        !    ! FINGERHAUS                !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG XG07-PG17 VERWENDET            *
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
      ***    VERTRAG-BEGINN
             10 VERTRAG-BEGINN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
040517***    ZAHLWEISE PG
             10 ZAHLWEISE-TEXT                    PIC  X(010).
             10 ZAHLWEISE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      *
      *** PRODUKT-DATEN.
          05 PRODUKT-DATEN.
      ***       GESAMTBETRAG
                15 GESAMTBETRAG.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
                   20 FELD-AENDERUNG-KZ           PIC  X(001).
      ***       UMSATZSTEUERSATZ
                15 UMST-SATZ.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
                   20 FELD-AENDERUNG-KZ           PIC  X(001).
      ***       UMSATZSTEUERBETRAG PG
                15 UMST-BETRAG.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
                   20 FELD-AENDERUNG-KZ           PIC  X(001).
      ***       RECHNUNGSNUMMER PG
                15 RECH-NR.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
                   20 FELD-AENDERUNG-KZ           PIC  X(001).
      ***       NORMIERTER PRODUKTGEBER-GESCHÄFTSVORFALL
                15 NPGV.
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


