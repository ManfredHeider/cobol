      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa0491.cpy  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/MF/CPY/DRPA0491.CPv  $
      *
      *   Rev 2.5   Mar 02 2021 11:20:42   t13ex67
      *Daisy-3267 Anpassungen Dialoge
      *
      *   Rev 2.4   Jun 09 2020 10:02:30   t13ex67
      *APA02908: Änderung der Abrechnungsweise beim Ersatzversicher
      *ungsgeschäft mit Umstellung von Ges. 11 auf Ges. 10 (betriff
      *finanz 2)
      *
      *   Rev 2.3   May 22 2018 07:41:56   t13ex67
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSS
      *
      *   Rev 2.2   Mar 01 2017 07:55:10   t13ex67
      *APA02275: DAISY-Erweiterungen zu FlexN - Dialoge MSS
      *
      *   Rev 2.1   11 Dec 2007 08:55:38   t13ae46
      *APA00622: Einführung Allfinanz (JWA)
      *
      *   Rev 2.0   Feb 27 1999 14:29:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Nov 1998 08:28:50   T01JM2S
      *PEG-PRODUKTIONSüBERGABE
      *
      *   Rev 1.0   23 Jul 1997 12:43:22   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA0491 ERWEITERUNG AMV-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: SCHRIEBER                                       *
      *    DATUM:    09.09.1997                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFPA0012!09.09.97!01.1! NEUERSTELLUNG             ! SCHRIEBER*
      *         !        !    !                           !          *
      * AFPA0012!10.07.98!01.2! PIN-PRÜFZIFFER UND        ! SCHRIEBER*
      *         !        !    ! AVERM-PRÜFZIFFER HINZUFÜGEN          *
      * APA00622!15.06.07! 2.1! VERTRIEBS-GES-BEZ10 HINZU ! WANG     *
      * APA02275!15.02.17! 2.2! NEUE FELDER FÜR FLEX-N    ! STORCH   *
      * APA02579!09.04.18! 2.3! VERTRIEBS-GES AUF LÄNGE 4 ! STORCH   *
      *         !        !    ! GEÄNDERT                  !          *
      * APA02908!20.02.20! 2.4! ERWEITERUNG UM VVNR'S     ! STORCH   *
      * DSY03267!19.01.21! 2.5! ERWEITERUNG KZ VORSCHÜSS. ! STORCH   *
      *         !        !    ! PROVISION                 !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG PG17 (PGPA012) VERWENDET.      *
      *    ES ENTHäLT ERWEITERUNGEN FüR DIE FELDER DES PEW-WORKFLOW  *
      *    - EINIGE BEZEICHNUNGEN UND TEXTE                          *
      *    - FüR EINGABEFELDER (INTENSIV, OFFEN UND AENDERUNG-KZ)    *
      *    - FüR AUSGABEFELDER (INTENSIV)                            *
      ****************************************************************
       01 AMV-DATEN-NEU-ERW.
          05 ANTRAGS-DATEN.
      ***    GESELLSCHAFT
             10 GESELLSCHAFT-BEZ10                PIC  X(010).
             10 PGEB-GES.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
150607*      10 VERTRIEBS-GES-BEZ10               PIC  X(010).
090418       10 VERTRIEBS-GES-BEZ                 PIC  X(004).
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
      ***    VORVERTRAGSNUMMER1
             10 VVNR1.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VORVERTRAGSNUMMER2
             10 VVNR2.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VORVERTRAGSNUMMER3
             10 VVNR3.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    VORVERTRAGSNUMMER4
             10 VVNR4.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    VORVERTRAGSNUMMER5
             10 VVNR5.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    VORVERTRAGSNUMMER6
             10 VVNR6.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER1
             10 GESVNR1.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER2
             10 GESVNR2.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER3
             10 GESVNR3.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER4
             10 GESVNR4.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER5
             10 GESVNR5.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
200220***    GESELLSCHAFT ZUR VORVERTRAGSNUMMER6
             10 GESVNR6.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    KUNDENNAME
             10 KUNDE-NAME.
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
      ***    VERTRAG-ABLAUF
             10 VERTRAG-ABLAUF.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    FAELLIG VON
             10 FAELLIG-VON.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    FAELLIG BIS
             10 FAELLIG-BIS.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    ZAHLWEISE
             10 ZAHLWEISE-BEZ10                   PIC  X(010).
             10 ZAHLWEISE.
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
      ***    ERSTELLDATUM
             10 ERST-DAT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    EINLOESEDATUM
             10 EINL-DAT-PG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    WäHRUNG
             10 WAEHRUNG-BEZ-KURZ                 PIC  X(004).
             10 WAEHRUNG.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
150217***    FLEX-N BERECHTIGT
             10 FLEX-N-B.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
150217***    FLEX-N RELEVANT
             10 FLEX-N-R.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
190121***    KENNZEICHEN VORSCHÜSSIGE PROVISION
             10 KZ-VOR-PRO.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      *** PRODUKT-TABELLE.
          05 PRODUKT-TABELLE.
             10 PRODUKT-ZEILE                     OCCURS 20.
      ***       ÄNDERUNGSKENNZEICHEN FüR PRODUKTZEILE
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***       GESCHAEFTSVORFALL
                15 PGV-PG.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PRODUKTSCHLüSSEL
                15 PROD-BEZ                       PIC  X(010).
                15 TARIF.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ***       PROVISIONSPFLICHTIGER BEITRAG
                15 PPFL-BEITRAG.
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
      ***       JAHRESNETTOPRAEMIE
                15 JAHRES-NT-PRAEMIE.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ******** Ende Copy-Member **************************************



