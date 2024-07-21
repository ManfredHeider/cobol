      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1333.CPY  $
      *    $Revision:   2.14  $
      *
      *    $Log:   L:/mf/CPY/DRKI1333.CPv  $
      *
      *   Rev 2.14   Nov 24 2020 09:16:44   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.13   Jun 25 2019 09:13:34   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.12   Jun 04 2019 14:44:46   t13ex39
      *AKI01670: Deutsche Bank - Erweiterung der Satzlängen für die
      * DTA (Kontokorrent und Kundenstamm)
      *
      *   Rev 2.9   Feb 19 2019 06:45:42   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.8   Feb 18 2019 16:29:08   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.7   Jan 30 2019 11:19:18   t13ex39
      *AKI01652: DeuBa TT für KUNDEN(VERTRAGSDATEN
      *
      *   Rev 2.6   Mar 14 2014 11:42:56   t13ex39
      *AKI01280: Bereinigung VWF um neu hinzukommende Deutsche Bank
      * Verträge
      *
      *   Rev 2.5   Sep 02 2013 12:14:44   t13ex14
      *AKI01204: SEPA - 'XX'-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.4   Sep 02 2013 09:57:26   t13ex14
      *AKI01204: SEPA - 'XX'-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.3   Jul 05 2004 10:33:18   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.2   Feb 25 2003 15:43:48   t13ex12
      *AKI00328 'XX'-Kunden ohne Anlagedepot
      *
      *   Rev 2.1   Jul 30 2002 16:48:32   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Dec 14 2001 09:20:42   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KUNDEN-/VERTRAGSDATEN VON DB              *
      *                                                                *
      *    SATZLÄNGE   450                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!13.03.02! Neuerstellung                    !v.d.Goltz  *
      * ---------------------------------------------------------------*
      *AKI00328!08.01.03! Neues Kennz. für Kunden ohne     !v.d.Goltz  *
      *        !        ! Depot. Namen DB24 in DB.         !           *
      *        !        ! Neues Feld für BLZ des Kundens.  !           *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      *AKI00082!01106.04! Neues Kennz. bei physischer Ver- !WHO        *
      *        !        ! tragslöschung Bei DEUTSCHER BANK !           *
      *----------------------------------------------------------------*
      *AKI01204!24.07.13!2.4 !NEUE FELDER EINGEFÜGT f. SEPA!WMI        *
      *        !        !    !  UND DEN FILLER ANGEPASST   !           *
      * -------+--------+----+-----------------------------+-----------*
      *AKI01280!10.03.14! 'XX'-VNR redefiniert             ! WHO       *
      * -------+--------+----+-----------------------------+-----------*
      *AKI01710!17.04.19! neue Felder Filialanschrift      ! WHO       *
      *        !        !                                  !           *
      *----------------------------------------------------------------*
      * AKI01805!14.10.20! NEUES ATTRIBUT ESAFE           ! ATAR       *
      *                                                                *
      *----------------------------------------------------------------*
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI1333.

      ***********************************************************
      *                                                         *
      *                 VORSATZ                                 *
      *                                                         *
      ***********************************************************

           10  'XX'-VORSATZ.
      *
               15  'XX'-V-SA            PIC X(001).
      *            SATZART = '1'
               15  'XX'-V-ABSENDER      PIC X(008).
      *            ABSENDER = 'DB AG'
               15  'XX'-V-DATUM         PIC 9(008).
      *            Buchungsdatum = 'JJJJMMTT'
               15  'XX'-V-EMPFAENGER    PIC X(004).
      *            EMPFAENGER = 'DVAG'
               15  'XX'-V-TEXT          PIC X(008).
      *            INHALTSANGABE:
      *            Tägliche Lieferung'KUNDENT'
      *            Komplettbestand   'KUNDENK'

               15  'XX'-V-WKZ           PIC X(003).
      *            WÄHRUNGSKENNZEICHEN = 'EUR'
               15  FILLER               PIC X(418).
      *            RESERVE
      *
      ***********************************************************
      *                                                         *
      *         HAUPTSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-SATZ     REDEFINES      'XX'-VORSATZ.
      *
               15  'XX'-SA                 PIC  X(001).
      *            SATZART        = '2'
               15  'XX'-GES                PIC  9(002).
      *            GESELLSCHAFT   = '48'
               15  'XX'-VNR.
                   20 FILIALNUMMER       PIC  9(003).
                   20 KUNDENNUMMER       PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               15  'XX'-VNR-1-10           REDEFINES 'XX'-VNR PIC 9(10).
               15  'XX'-VBNR               PIC  9(007).
      *            ABSCHLUSSVERMITTLER
               15  'XX'-VBEGINN            PIC  9(008).
      *            DATUM VERTRAGSBEGINN/Kundeneröffnung
               15  'XX'-VABLAUF            PIC  9(008).
      *            DATUM VERTRAGSABLAUF/Kundenschließung
               15  'XX'-INFOSPERRE-KZ      PIC  X(001).
      *            S = INFOSPERRE GESETZT
               15  'XX'-INFOSPERRE-DATUM   PIC  9(008).
      *            DATUM der letzen Änderung für Datenschutzsperre
               15  'XX'-VERFUEG-KZ         PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
      *              O = Einzelverfügung
      *              U = Gemeinchaftsverfügung
               15  'XX'-NACHLASS-KZ        PIC  X(001).
      *            KENNZEICHEN NACHLAS
      *              J = Nachlass
               15  'XX'-TELREVERS          PIC  X(001).
      *            Telefonrevers = 'J'
               15  'XX'-ONLREVERS          PIC  X(001).
      *            Onlinerevers = 'J'
               15  'XX'-PERSON-LFDNR       PIC  9(002).
      *            '01' für erster Kontoinhaber
               15  'XX'-NNAME              PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
               15  'XX'-VNAME              PIC  X(035).
      *                VORNAME DES KONTOINHABERS
               15  'XX'-GEBDAT             PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
               15  'XX'-GESCHLECHT         PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
               15  'XX'-ANREDE             PIC  X(001).
      *                ANREDE...........*1 : HERRN
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*3 : FRAEULEIN ??
      *                ANREDE...........*4 : HERRN und FRAU ??
      *                ANREDE...........*5 : HERREN ??
      *                ANREDE...........*6:  AN ??
      *                ANREDE...........*0 : KEINE ANREDE

               15  'XX'-STR                PIC  X(028).
      *                STRASSE
               15  'XX'-HAUSNR             PIC  X(009).
      *                Haus-Nummer
               15  'XX'-POSTFACH           PIC  X(009).
      *                POSTFACH
               15  'XX'-PLZ                PIC  X(009).
      *                POSTLEITZAHL
               15  'XX'-ORT                PIC  X(028).
      *                ORT
               15  'XX'-AUSL-KZ            PIC  X(003).
      *                AUSLANDSKENNZEICHEN
               15  'XX'-VORWAHL-TEL        PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
               15  'XX'-TEL                PIC  9(009).

               15  'XX'-DEPOT-AKTIV        PIC  9(001).
      *              Kennz. = 1, Kunde hat aktives Depot
      *              Kennz. = 0, Kunde hat kein Depot

               15  'XX'-BLZ                PIC  9(008).
      *              BLZ des Kundenkontos

               15  'XX'-DEL-KZ             PIC  X(001).
      *              DEL-Kennzeichen
      *
               15  'XX'-BIC                PIC  X(011).
      *            BIC des Kundenkontos

               15  'XX'-KND-NAME           PIC  X(028).
      *            Kundenname
               15  'XX'-FILIALE-ART        PIC  X(035).
      *            Art der Filiale
               15  'XX'-FILIALE-NAME       PIC  X(035).
      *            Name der Filiale
               15  'XX'-FILIALE-STR        PIC  X(035).
      *            Adresse (Straße und Hausnr.)        .
               15  'XX'-FILIALE-ORT        PIC  X(035).
      *            Adresse (PLZ und Ort)
               15  'XX'-ESAFE              PIC  X(001).
      *            ESAFE = 'J'
               15  'XX'-REERVER            PIC  X(028).
      *            RESERVE
      ***********************************************************
      *                                                         *
      *          NACHSATZ                                       *
      *                                                         *
      ***********************************************************
      *
           10  'XX'-NACHSATZ    REDEFINES     'XX'-VORSATZ.
      *
               15  'XX'-N-SA               PIC  X(001).
      *            SATZART             'E'
               15  'XX'-N-ANZ              PIC  9(007).
      *            ANZAHL SAETZE ohne VOR- UND NACHSATZ
               15  FILLER                  PIC  X(442).
      *            RESERVE







