      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2012
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2675.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI2675.CPv  $
      *
      *   Rev 2.1   Oct 18 2012 10:14:38   t13ex52
      *AKI01147: Central-Tarifumstellung (Bordero-Tool)
      *
      *   Rev 2.0   Sep 06 2012 11:00:46   t13ex52
      *Archiverstellung
      *----------------------------------------------------------------*
      ******************************************************************
      *                     #####################                      *
      *             ########   D R K I 2 6 7 5   ########              *
      *                     #####################                      *
      *                                                                *
      *  COPY-MEMBER FUER ErmittlungsPROGRAMM, erstellt fuer die       *
      *  Uebernahme der Daten nach Abloesung des BORDERO-Tools         *
      *  bei der CKV.                                                  *
      *    Tabelle CKVVERVORSCHLAG (TCKI0450) / KRANKEN_VORSCHLAG      *
      *                                                                *
      *    SATZLÄNGE  70                                               *
      *                                                                *
      *----------------------------------------------------------------*
      *    LOGBUCH                                                     *
      *DATUM   !     AENDERUNG                         !NAME       !VER*
      *AUFTRAG !                                       !           !   *
      *--------!---------------------------------------!-----------!---*
      *17.09.12!NEUERSTELLUNG                          !Schlenther !01 *
      *AKI01147!                                       !           !   *
      *--------!---------------------------------------!-----------!---*
      *        !                                       !           !   *
      ******************************************************************
       01  DRKI2675.
           05 VVS-AKTUAL-KZ                  PIC X(01).
      *        I = INSERT (nur dieser Wert ist moeglich)
      **
           05 VVS-VERTRAGSID                 PIC X(13).
      ** kuenstlicher Schluessel
           05 VVS-VERTRAGSNUMMER             PIC X(15).
      ** Vertragsnummer der Partner
           05 VVS-VTRGERHOEHPRZ              PIC S9(07)V99.
      ** Beitragserhöhung (v.H.) für den Vertrag
           05 VVS-VTRGBEITRRUEK              PIC S9(07)V99.
      ** Betrag Beitragsrückerstattung für den Vertrag in Euro-Betrag
           05 VVS-KUNDBINDVTRG               PIC X.
      ** Kundenbindung Vertrag
           05 VVS-STORNOHAFTUNG              PIC X.
      ** In Stornohaftung
           05 VVS-FILLER                     PIC X(21).
      *
      ******************************************************************
