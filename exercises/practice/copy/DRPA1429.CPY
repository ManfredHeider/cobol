      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1429.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRPA1429.CPv  $
      *
      *   Rev 2.1   07 Nov 2000 16:08:04   T13EX21
      *APA00112 - Neuerstellung
      *
      *   Rev 2.0   12 Jul 2000 12:32:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1429 ERWEITERUNG DIT-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    10.07.2000                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00098!10.07.00!02.1! NEUERSTELLUNG             ! STORCH   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG XG03-PG17 VERWENDET            *
      *    ES ENTHÄLT ERWEITERUNGEN FÜR DIE FELDER DES               *
      *    DIT-DATEN-WORDKFLOW.                                      *
      *    - EINIGE BEZEICHNUNGEN UND TEXTE                          *
      *    - FÜR EINGABEFELDER (INTENSIV, OFFEN UND AENDERUNG-KZ)    *
      *    - FÜR AUSGABEFELDER (INTENSIV)                            *
      ****************************************************************
       01 DIT-DATEN-NEU-ERW.
          05 PM-DATEN.
      ***    GESELLSCHAFT
             10 GESELLSCHAFT-BEZ10                PIC  X(010).
             10 PGEB-GES.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    PIN + PRÜFZIFFER
             10 PIN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
             10 PINPZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    EXT. VERTRAGSNUMMER
             10 VNR.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    KUNDENNAME
             10 KUNDE-NNAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    KUNDEN-VORNAME
             10 KUNDE-VNAME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VERTRAG-BEGINN
             10 VERTRAG-BEGINN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    FAELLIG VON
             10 FAELLIG-VON.
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
      ***    WÄHRUNG
             10 WAEHRUNG-BEZ-KURZ                 PIC  X(004).
             10 VERTRAG-WAEHRUNG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    ZAHLWEISE PG
             10 ZAHLWEISE-TEXT                    PIC  X(010).
             10 ZAHLWEISE.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      ***    SPARBETRAG
             10 SPARBETRAG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
      *
      *** PRODUKT-DATEN.
          05 PRODUKT-DATEN.
      ***       PRODUKTGEBER-GESCHÄFTSVORFALL
                15 PGV-PG.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
                15 PGV-KZ                         PIC  X(001).
      ***       WERTPAPIERKENNNR.
                15 WPKN.
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
