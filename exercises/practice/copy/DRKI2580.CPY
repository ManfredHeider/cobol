      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  13.03.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2580.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2580.CPv  $
      *
      *   Rev 2.4   Jan 14 2021 15:13:36   t13ae31
      *AKI01823: G50 Vertragsanzeige Tarife mehrfach Pgme
      *
      *   Rev 2.3   Sep 04 2020 11:57:04   t13ac20
      *AKI01799: Central: Aufnahme des EBE Beitrags in die Bestands
      *daten - Source
      *
      *   Rev 2.2   Mar 06 2018 10:15:12   t13ac13
      *AKI01568: Central: Bestandserweiterung um ein Kennz. für Anw
      *artschaft - Copys
      *
      *   Rev 2.1   Jun 26 2012 17:04:26   t13ex52
      *AKI01100: Central Bestandsdaten täglich aktualisieren
      *
      *   Rev 2.0   Mar 13 2012 19:24:58   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 5 8 0   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Umstellung der CKV-Verarbeitung woechentlich Komplettbestand  *
      *  auf die CKV-Verarbeitung taeglich Deltabestand.               *
      *    AUFBEREITUNG DER Daten fuer die DB2/Tabelle                 *
      *                                    KrankenTarifArtBez(TCKI0378)*
      *        - AUSGABE: KRTARIFARTBEZ fuer alle Gesellschaften       *
      *                                                                *
      *    SATZLAENGE   96 + 56 Filler = 152                           *
      *                                                                *
      *                                                                *
      ******************************************************************
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME      !VERS.*
      *AUFTRAG !                                      !          !     *
      *--------!--------------------------------------!----------!-----*
      *25.04.12!NEUERSTELLUNG (Kopie von DRKI0610 aus !Schlenther! 1.0 *
      *AKI01100!Produktion vor Umstellung NKDA)       !          !     *
      *--------!--------------------------------------!----------!-----*
      *16.03.18!Kennzeichen STVTBTG (Anwartschaft)    ! Boost    ! 1.1 *
      *AKI01568!neu                                   !          !     *
      *--------!--------------------------------------!----------!-----*
      *AKI01799! neues Feld  BE-REDUK-BTG             ! T13AC20  !     *
      *--------!--------------------------------------!----------!-----*
      *AKI01823! neues Feld  ANZ-TAR-DUP              ! HHU      !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      *        !                                      !          !     *
      ******************************************************************
      * KRTARIFARTBEZ
      **
        01 DRKI2580.
          05 KTAR-AKTUAL-KZ                PIC X(01).
      *
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
          05 KTAR-GESELLSCHFT              PIC 9(02).
      ** Wird vom ErmittlungsPGM immer mit "50" (CENTRAL) gefuellt.
          05 KTAR-VTRGNR                   PIC 9(15).
          05 KTAR-PERSONENNR               PIC 9(04).
          05 KTAR-DATABGANGVERTR           PIC 9(08).
          05 KTAR-DATABGANGVERTR-AENDKZ    PIC X(01).
          05 KTAR-DATZUGANGVERTR           PIC 9(08).
          05 KTAR-DATZUGANGVERTR-AENDKZ    PIC X(01).
          05 KTAR-MONATSBEITRAG            PIC S9(9)V9(2).
          05 KTAR-MONATSBEITRAG-AENDKZ     PIC X(01).
          05 KTAR-SELBSTBETEILIG           PIC S9(9)V9(2).
          05 KTAR-SELBSTBETEILIG-AENDKZ    PIC X(01).
          05 KTAR-TARIFNAMEN               PIC X(08).
          05 KTAR-TARIFNAMEN-AENDKZ        PIC X(01).
          05 KTAR-RISIKOZUSCHLAG           PIC X(10).
          05 KTAR-RISIKOZUSCHLAG-AENDKZ    PIC X(01).
          05 KTAR-VERSLEISTUNG             PIC S9(9)V9(2).
          05 KTAR-VERSLEISTUNG-AENDKZ      PIC X(01).
160318    05 KTAR-STVTBTG                  PIC X(02).
160318    05 KTAR-STVTBTG-AENDKZ           PIC X(01).
          05 KTAR-BE-REDUK-BTG             PIC S9(9)V9(2).
          05 KTAR-BE-REDUK-BTG-AENDKZ      PIC X(01).
          05 KTAR-FILLER                   PIC X(38).
          05 KTAR-ANZ-TAR-DUP              PIC 99.
          05 KTAR-ANZ-TAR-DUP-AENDKZ       PIC X(01).
      ******************************************************************
      *        E N D E  Copy-Buch   DRKI2580  KrTarifArtBez            *
      ******************************************************************



