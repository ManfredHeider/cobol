      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  23.11.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2542.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2542.CPv  $
      *
      *   Rev 2.2   Feb 23 2012 11:12:30   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.1   Feb 03 2012 15:01:28   t13ex19
      *AKI01102: SUH neues Produkt PUR
      *
      *   Rev 2.0   Nov 23 2011 15:41:48   t13ex19
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM AUFBEREITUNGSPROGRAMM                       *
      *     - AUSGABEDATEI REOB fuer die Tabelle UNFBEITRRUECK         *
      *         Aenderungskennzeichen auf Feldebene                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *AKI01102! NEU                                  ! BST     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *                             R E O B                            *
      *                      AUSGABE-DATENSATZ                         *
      *     LAENGE: 210 Bytes                   STAND: 21.08.2007      *
      *                                                                *
      **************************************************************** *

       01  DRKI2542.

                 05  REOB-AUS-617-VERARB-KNZ           PIC X(01).
      **            Verarbeitungskennzeichen
      **            I = Insert U = Update D = Delete

                 05  REOB-AUS-617-VERTRAGS-ID          PIC X(13).

                 05  REOB-AUS-617-CVNR                 PIC 9(15).
                 05  REOB-AUS-617-CVEZHOSTNR           PIC X(15).

      ***  Schlüssel: Komponentengruppennummer FeldNr. 141
                 05  REOB-AUS-617-RGID                 PIC 9(04).

      ***  Schlüssel: laufendes Kalenderjahr FeldNr.145
                 05  REOB-AUS-617-JAHR                 PIC 9(04).

      ***  Schlüssel: laufender Kalendermonat FeldNr. 146
                 05  REOB-AUS-617-MONAT                PIC 9(02).

      ***  Datenfeld: laufender Vertragsmonat (1-144) FeldNr. 143
                 05  REOB-AUS-617-MONNR                PIC 9(04).
                 05  REOB-AUS-617-MONNR-UPD            PIC X.
      ***  J = Änderung laufender Vertragsmonat

      ***  Datenfeld: laufende Monatnummer pro Jahr FeldNr. 144
                 05  REOB-AUS-617-LFDMON               PIC 9(02).
                 05  REOB-AUS-617-LFDMON-UPD           PIC X.
      ***  J = Änderung laufender Monat pro jahr

      ***  Datenfeld: Status des Datensatzes FeldNr. 147
                 05  REOB-AUS-617-STATUS               PIC X.
                 05  REOB-AUS-617-STATUS-UPD           PIC X.
      ***  J = Änderung Status des Datensatzes

      ***  Datenfeld: Erstelldatum Datensatz  FeldNr. 148
                 05  REOB-AUS-617-ERSTDATUM            PIC X(10).
                 05  REOB-AUS-617-ERSTDATUM-UPD        PIC X.
      ***  J = Änderung Erstelldatum Datensatz

      ***  Datenfeld: garantierter Rückkaufswert FeldNr.149
                 05  REOB-AUS-617-GARRUEWERT           PIC 9(11)V9(04).
                 05  REOB-AUS-617-GARRUEWERT-UPD       PIC X.
      ***  J = Änderung garantierter Rückkaufswert

      ***  Datenfeld: garantierte Todesfallleistung FeldNr. 150
                 05  REOB-AUS-617-GARTODLEIST          PIC 9(11)V9(04).
                 05  REOB-AUS-617-GARTODLEIST-UPD      PIC X.
      ***  J = Änderung garantierte Todesfallleistung

      ***  Datenfeld: Rückkaufswert incl. Überschuss  FeldNr. 151
                 05  REOB-AUS-617-RUEWERTUEB           PIC 9(11)V9(04).
                 05  REOB-AUS-617-RUEWERTUEB-UPD       PIC X.
      ***  J = Änderung Rückkaufswert incl. Überschuss

      ***  Datenfeld: Todesfallleistung incl. Überschuss FeldNr. 152
                 05  REOB-AUS-617-TODLEIUEB            PIC 9(11)V9(04).
                 05  REOB-AUS-617-TODLEIUEB-UPD        PIC X.
      ***  J = Änderung Todesfalleistung incl. Überschuss

      ***  Datenfeld: stille Reserve FeldNr. 153
                 05  REOB-AUS-617-STILLERES            PIC 9(11)V9(04).
                 05  REOB-AUS-617-STILLERES-UPD        PIC X.
      ***  J = Änderung stille Reserve

      ***  Datenfeld: Rechnungszins FeldNr. 154
                 05  REOB-AUS-617-RECHZINS             PIC 9(02)V9(02).
                 05  REOB-AUS-617-RECHZINS-UPD         PIC X.
      ***  J = Änderung Rechnungszins

      ***  interne laufende Nummer
                 05  REOB-AUS-617-INTLFDNR             PIC 9(04).

                 05  FILLER                            PIC X(36).

      *---------------------------------------------------------------*
