      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  03.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI3747.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRKI3747.CPv  $
      *
      *   Rev 2.1   Feb 25 2020 13:33:54   t13ex78
      *AKI01731: Programme und Copies
      *
      *   Rev 2.0   Dec 03 2019 10:59:44   t13ex78
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *                                                                *
      * G66 - Santander Consumerbank                                   *
      *    Vertrags- und Privatkredit DTA                              *
      *                                                                *
      *    Satzlänge 400 Bytes                                         *
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
       01  'XX'-DTA-SATZ.
      *
      *****************************************************************
      * Vorsatz
      *****************************************************************
      *
           05  'XX'-VORSATZ.
      * Satzart - immer 'A'
               10  'XX'-V-SA                       PIC  X(001).
      *
      * Absender - 'SCB BANK'
               10  'XX'-V-ABSENDER                 PIC  X(008).
      *
      * Datum der Erstellung des DTA im Format  JHJJMMTT
               10  'XX'-V-DATUM                    PIC  9(008).
      *
      * Empfänger bzw. Mandant - 'DVAG', 'ALLF' oder 'ALAG'
               10  'XX'-V-EMPFAENGER               PIC  X(004).
      *
      * Welche Art Lieferung?
      * 'PK-K' - Komplettlieferung
      * 'PK-D' - Deltalieferung
               10  'XX'-V-LIEFERUNG                PIC  X(008).
      *
      * Reserve
               10  FILLER                          PIC  X(371).
      *
      *****************************************************************
      * Hauptsatz
      *****************************************************************
 
           05  'XX'-HAUPTSATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'C'
               10  'XX'-SA                         PIC  X(001).
      *
      * Gesellschaft - immer '66'
               10  'XX'-GESELLSCHAFT               PIC  9(002).
      *
      * Vertragsnummer mit führenden Nullen
               10  'XX'-VNR                        PIC  X(015).
      *
      * Vertragszustand
      * 'A' - aktiv
      * 'D' - abgelaufen/storniert
               10  'XX'-ZUSTAND                    PIC  X(001).
      *
      * Beginndatum im Format JJJJMMTT
               10  'XX'-BEGINNDAT                  PIC  9(008).
      *
      * Endedatum im Format JJJJMMTT
               10  'XX'-ENDEDAT                    PIC  9(008).
      *
      * VB-Nummer
               10  'XX'-VBNR                       PIC  9(007).
      *
      * Darlehensbetrag
               10  'XX'-DARLEHENBETRAG             PIC  9(012)V9(002).
      *
      * Datum der Kreditzusage
               10  'XX'-ZUSAGEDAT                  PIC  9(008).
      *
      * Datum der Kreditauszahlung im Format JJJJMMTT
               10  'XX'-AUSZAHLDAT                 PIC  9(008).
      *
      * Kreditart - immer 'PKS'
               10  'XX'-KREDITART                  PIC  X(006).
      *
      * aktueller Saldo
               10  'XX'-SALDO                      PIC  9(012)V9(002).
      *
      * Sollzinssatz p.a. in %
               10  'XX'-SOLLZINS                   PIC  9(002)V9(004).
      *
      * Rate in EUR
               10  'XX'-RATE                       PIC  9(012)V9(002).
      *
      * Datum der Fälligkeit im Format JJJJMMTT
               10  'XX'-FAELLIGDAT                 PIC  9(008).
      *
      * Sicherheitspaket J oder N
               10  'XX'-SICHERHEIT                 PIC  X(001).
      *
      * Restlaufzeit in Monaten
               10  'XX'-RESTLAUFZEIT               PIC  9(003).
      *
      * IBAN Belastungskonto
               10  'XX'-IBAN                       PIC  X(034).
      *
      * BIC Belastungskonto
               10  'XX'-BIC                        PIC  X(011).
      *
      * Mandatsreferenz
               10  'XX'-MANDATSREF                 PIC  X(035).
      *
      * Kontoinhaber Belastungskonto - Nachname
               10  'XX'-BTGZ-NACHNAME              PIC  X(030).
      *
      * Kontoinhaber Belastungskonto - Vorname
               10  'XX'-BTGZ-VORNAME               PIC  X(030).
      *
      * IBAN des Kreditkontos selbst
               10  'XX'-IBAN-DES-PK                PIC  X(034).
      * Reserve
               10  FILLER                          PIC  X(102).
 
      *****************************************************************
      * Endesatz
      *****************************************************************
      *
           05  'XX'-ENDESATZ REDEFINES 'XX'-VORSATZ.
      *
      * Satzart - immer 'E'
               10  'XX'-N-SA                       PIC  X(001).
      *
      * Satzanzahl - gelieferte C-Sätze
               10  'XX'-N-ANZAHL                   PIC  9(008).
      *
      * Reserve
               10  FILLER                          PIC  X(391).



