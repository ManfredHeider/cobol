      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3749.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3749.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 13:34:54   t13ex78
      *AKI01731: Programme und Copies
      *
      *   Rev 2.0   Dec 03 2019 11:00:12   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      * G66 - Santander Consumerbank                                   *
      *    Vertrags- und Privatkreditdaten - Ausgabe Ermittlung*
      *                                                                *
      *    Satzlänge 500 Bytes                                         *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI01731!14.01.20!2.0 ! Neu                         ! t13ex78   *
      *        !        !    !                             !           *
      *                                                                *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
       01  'XX'-AEND-SATZ.
      *
      * Änderungskennzeichen - I, U oder D
           10  'XX'-AEND-AKTU-KZN              PIC  X(001).
      *
      * Gesellschaft - immer '66'
           10  'XX'-AEND-GESELLSCHAFT          PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
           10  'XX'-AEND-VNR                   PIC  X(015).
      *
      * Vertragszustand
      * 'A' - aktiv
      * 'D' - abgelaufen/storniert
           10  'XX'-AEND-ZUSTAND               PIC  X(001).
           10  'XX'-AEND-ZUSTAND-KZN           PIC  X(001).
      *
      * Beginndatum im Format JJJJMMTT
           10  'XX'-AEND-BEGINNDAT             PIC  9(008).
           10  'XX'-AEND-BEGINNDAT-KZN         PIC  X(001).
      *
      * Endedatum im Format JJJJMMTT
           10  'XX'-AEND-ENDEDAT               PIC  9(008).
           10  'XX'-AEND-ENDEDAT-KZN           PIC  X(001).
      *
      * VB-Nummer - keine Änderungsmöglichkeit vorgesehen
           10  'XX'-AEND-VBNR                  PIC  9(007).
      *
      * Darlehensbetrag
           10  'XX'-AEND-DARLBETRAG            PIC  9(012)V9(002).
           10  'XX'-AEND-DARLBETRAG-KZN        PIC  X(001).
      *
      * Datum der Kreditzusage
           10  'XX'-AEND-ZUSAGEDAT             PIC  9(008).
           10  'XX'-AEND-ZUSAGEDAT-KZN         PIC  X(001).
      *
      * Datum der Kreditauszahlung im Format JJJJMMTT
           10  'XX'-AEND-AUSZAHLDAT            PIC  9(008).
           10  'XX'-AEND-AUSZAHLDAT-KZN        PIC  X(001).
      *
      * Kreditart - immer 'PKS' - keine Änderungsmöglichkeit gegeben
           10  'XX'-AEND-KREDITART             PIC  X(006).
      *
      * aktueller Saldo
           10  'XX'-AEND-SALDO                 PIC  9(012)V9(002).
           10  'XX'-AEND-SALDO-KZN             PIC  X(001).
      *
      * Sollzinssatz p.a. in %
           10  'XX'-AEND-SOLLZINS              PIC  9(002)V9(004).
           10  'XX'-AEND-SOLLZINS-KZN          PIC  X(001).
      *
      * Rate in EUR
           10  'XX'-AEND-RATE                  PIC  9(012)V9(002).
           10  'XX'-AEND-RATE-KZN              PIC  X(001).
      *
      * Datum der Fälligkeit im Format JJJJMMTT
           10  'XX'-AEND-FAELLIGDAT            PIC  9(008).
           10  'XX'-AEND-FAELLIGDAT-KZN        PIC  X(001).
      *
      * Sicherheitspaket J oder N
           10  'XX'-AEND-SICHERHEIT            PIC  X(001).
           10  'XX'-AEND-SICHERHEIT-KZN        PIC  X(001).
      *
      * Restlaufzeit in Monaten
           10  'XX'-AEND-RESTLAUFZEIT          PIC  9(003).
           10  'XX'-AEND-RESTLAUFZEIT-KZN      PIC  X(001).
      *
      * IBAN Belastungskonto
           10  'XX'-AEND-IBAN                  PIC  X(034).
           10  'XX'-AEND-IBAN-KZN              PIC  X(001).
      *
      * BIC Belastungskonto
           10  'XX'-AEND-BIC                   PIC  X(011).
           10  'XX'-AEND-BIC-KZN               PIC  X(001).
      *
      * Mandatsreferenz
           10  'XX'-AEND-MANDATSREF            PIC  X(035).
           10  'XX'-AEND-MANDATSREF-KZN        PIC  X(001).
      *
      * Kontoinhaber Belastungskonto - Nachname
           10  'XX'-AEND-BTGZ-NACHNAME         PIC  X(030).
           10  'XX'-AEND-BTGZ-NACHNAME-KZN     PIC  X(001).
      *
      * Kontoinhaber Belastungskonto - Vorname
           10  'XX'-AEND-BTGZ-VORNAME          PIC  X(030).
           10  'XX'-AEND-BTGZ-VORNAME-KZN      PIC  X(001).
      *
      * IBAN des Kreditkontos selbst - keine Änderungsmöglichkeit
      * gegeben
           10  'XX'-AEND-IBAN-DES-PK                PIC  X(034).
      *
      * Reserve
           10  FILLER                          PIC  X(093).
 



