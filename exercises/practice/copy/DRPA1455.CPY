      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drpa1455.cpy  $
      *    $Revision:   2.11  $
      *
      *    $Log:   L:/MF/CPY/DRPA1455.CPv  $
      *
      *   Rev 2.11   Mar 02 2021 11:21:08   t13ex67
      *Daisy-3267 Anpassungen Dialoge
      *
      *   Rev 2.10   May 22 2018 07:42:34   t13ex67
      *APA02579: FriWi: Provisionsverteilung- und Abrechnung - MSS
      *
      *   Rev 2.9   Sep 14 2017 11:13:58   t13ex67
      *APA02504: Ges 48- Vorbewegungsermittlung nach T-Nummern - PG
      *17/PEG MSS
      *
      *   Rev 2.8   Jan 15 2016 09:10:14   t13ae46
      *APA02069: Kundenbetreuungsprovision Mein Gesundheitsplan
      *
      *   Rev 2.7   11 Dec 2007 08:56:10   t13ae46
      *APA00622: Einführung Allfinanz (JWA)
      *
      *   Rev 2.6   May 19 2003 10:38:14   t13ex21
      *APA00235 - Bruttoanlagesumme als Eingabefeld
      *
      *   Rev 2.5   Aug 07 2002 15:30:46   t13ex21
      *APA00207 - Erweiterung für Feld Belastungsdauer
      *
      *   Rev 2.4   Jun 03 2002 10:17:50   t13ex21
      *APA00191 - Neues Feld Laufzeit
      *
      *   Rev 2.3   06 Sep 2001 08:49:04   t13ex21
      *APA00133 - Neues Feld Vertragsablauf
      *
      *   Rev 2.2   09 Mar 2001 12:08:04   T13EX21
      *APA00131 - Erweiterung für AML-Darlehen
      *
      *   Rev 2.1   05 Feb 2001 09:53:58   T13EX21
      *APA00118 - Neuerstellung
      *
      *   Rev 2.0   04 Jan 2001 11:48:16   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1455 ERWEITERUNG NPM-DATEN               *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    05.01.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * APA00118!05.01.01!02.1! NEUERSTELLUNG             ! STORCH   *
      * APA00131!05.03.01!02.2! NEUES FELD: FÄLLIG-BIS    ! STORCH   *
      * APA00133!03.09.01!02.3! NEUES FELD: VERTRAGSENDE  ! STORCH   *
      * APA00191!31.05.02!02.5! NEUES FELD: LAUFZEIT      ! STORCH   *
      * APA00207!25.06.02!02.6! NEUE FELDER: BERECHSDAUER,! STORCH   *
      *         !        !    ! PPFL. BEGINN              !          *
      * APA00235!13.05.03!02.7! NEUES FELD: BRUTTOANLAGE- ! STORCH   *
      *         !        !    ! SUMME                     !          *
      * APA00622!15.06.07!02.7! NEUES FELD: VERTRIEBS-    !          *
      *         !        !    ! GES-BEZ10                 ! WANG     *
      * APA02069!27.11.15!02.8! VERSICHERTE-PERSON NEU    ! WANG     *
      * APA02504!10.08.17!02.9! NEUES FELD: T-NUMMER      ! STORCH   *
      * APA02579!09.04.18!2.10! VERTRIEBS-GES AUF LÄNGE 4 ! STORCH   *
      *         !        !    ! GEÄNDERT                  !          *
      * DSY-3267!19.01.21!2.11! NEU: KZ VORSCHÜSS. PROV.  ! STORCH   *
      * DSY-3472!21.01.21!2.12! NEU: TARIF                ! STORCH   *
      * DSY-4340!11.01.22!    ! NEU: SONDERHAFT-KZ        ! STORCH   *
      * DSY-4566!24.05.22!    ! NEU: VERTRAGSTEIL         ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY WIRD IM DIALOG XG05-PG17 VERWENDET            *
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
             10 VERTRIEBS-GES-BEZ                 PIC  X(004).
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
      ***    VERTRAG-ENDE
             10 VERTRAG-ENDE.
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
      ***    VERTRAGSSPLITTUNGS-KENNZEICHEN
             10 VSPLITKZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    LAUFZEIT
             10 LAUFZEIT.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    BERECHNUNGSDAUER
             10 BERECHNUNGSDAUER.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    PPFL. BEGINN (EINLÖSEDATUM MIT ZUSATZINFO=3)
             10 PPFL-BEGINN.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    BRUTTOANLAGESUMME
             10 BRUTTO-SUMME.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    VERSICHERTE PERSON
             10 VERSICHERTE-PERSON.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
      ***    T-NUMMER
             10 T-NUMMER.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
190121***    KENNZEICHEN VORSCHÜSSIGE PROVISION
             10 KZ-VOR-PRO.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
110122***    SONDERHAFT-KENNZEICHEN
             10 SONDERHAFT-KZ.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
240522***    VERTRAGSTEIL
             10 VERTRAGSTEIL.
                15 FELD-INTENSIV                  PIC  X(001).
                15 FELD-OFFEN                     PIC  X(001).
                15 FELD-AENDERUNG-KZ              PIC  X(001).
240522***    JAHRESNETTOPRAEMIE
             10 JAHRESNETTOPRAEMIE.
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
      ***       PRODUKT
                15 PRODUKT.
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
210421***       TARIF
                15 TARIF.
                   20 FELD-INTENSIV               PIC  X(001).
                   20 FELD-OFFEN                  PIC  X(001).
      ******** ENDE COPY-MEMBER **************************************
