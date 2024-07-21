      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1350.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI1350.CPv  $
      *
      *   Rev 2.8   Nov 24 2020 09:18:16   u130685
      *AKI01805: Aufnahme Flag 'eSafe' Tabelle Struktur
      *
      *   Rev 2.7   Sep 02 2013 12:14:38   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.6   Sep 02 2013 09:57:20   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.5   Jan 27 2005 14:56:20   t13ex39
      *FKI00887: Falscher VB in Tabelle GUTHABENPRODUKT
      *
      *   Rev 2.4   Aug 23 2004 12:25:22   t13ex12
      *AKI00430: Löschen von weiteren Kontoinh.
      *
      *   Rev 2.3   Jul 13 2003 23:45:58   T13ex12
      *AKI00327 Einführung Deutsche Bank Produkte
      *
      *   Rev 2.2   Feb 25 2003 15:47:38   t13ex12
      *AKI00328 DB-Kunden ohne Anlagedepot
      *
      *   Rev 2.1   Jul 30 2002 16:48:40   t13ex12
      *AKI00274 Einführung DB24/DWS
      *
      *   Rev 2.0   Mar 12 2002 12:33:54   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS KUNDEN-/VERTRAGSDATEN VON DB         *
      *                                                                *
      *    SATZLÄNGE   300                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00274!11.04.02! Neuerstellung                    !v.d.Goltz  *
      *----------------------------------------------------------------*
      *AKI00328!08.01.03! Neues Kennz. für Kunden ohne     !v.d.Goltz  *
      *        !        ! Depot. Namen DB24 in DB          !           *
      * ---------------------------------------------------------------*
      *AKI00327!28.05.03! Auftragsnr. und Datum in Änd.-   !v.d.Goltz  *
      *        !        ! Zeilen haben bei letzter Prod.-  !           *
      *        !        ! Übergabe gefehlt.                !           *
      * ---------------------------------------------------------------*
      * AKI00430!09.08.04! Löschungs-KZ aufnehmen, wg. Änd.! v.d.Goltz *
      *         !        ! in PMKI0447.                    !           *
      * ---------------------------------------------------------------*
      * FKI00887!25.01.05! KSDS-VNR redefiniert            ! Hornung   *
      *----------------------------------------------------------------*
      *2AKI01204!24.07.13! NEUE FELDER EINGEFÜGT f. SEPA   !WMI        *
      *         !        !   UND DEN FILLER ANGEPASST      !           *
      *         !        !                                 !           *
      *---------!--------!---------------------------------!-----------*
      *AKI01805 !14.10.20! NEUES ATTRIBUT ESAFE           ! ATAR       *
      *         !        !                                !            *
      * ENDE-LOGBUCH                                                   *
      ******************************************************************
      *
CCC009*    SKIP1
       01  DRKI1350.

           05  KSDS-SA                   PIC  X(001).
      *            SATZART        = '2'
           05  KSDS-GES                  PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-VNR-1-10             PIC  9(10).
           05  KSDS-VNR REDEFINES KSDS-VNR-1-10.
      *    05  KSDS-VNR.
               10 FILIALNUMMER           PIC  9(003).
               10 KUNDENNUMMER           PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
           05  KSDS-VBNR                 PIC  9(007).
      *            ABSCHLUSSVERMITTLER
           05  KSDS-VBEGINN              PIC  9(008).
      *            DATUM VERTRAGSBEGINN/Kundeneröffnung
           05  KSDS-VABLAUF              PIC  9(008).
      *            DATUM VERTRAGSABLAUF/Kundenschließung
           05  KSDS-INFOSPERRE-KZ        PIC  X(001).
      *            S = INFOSPERRE GESETZT
           05  KSDS-INFOSPERRE-DATUM     PIC  9(008).
      *            DATUM der letzen Änderung für Datenschutzsperre
           05  KSDS-VERFUEG-KZ           PIC  X(001).
      *            KENNZEICHEN VERFÜGUNG
      *              O = Einzelverfügung
      *              U = Gemeinchaftsverfügung
           05  KSDS-NACHLASS-KZ          PIC  X(001).
      *            leer
           05  KSDS-TELREVERS            PIC  X(001).
      *            leer
           05  KSDS-ONLREVERS            PIC  X(001).
      *            leer
           05  KSDS-PERSON-LFDNR         PIC  9(002).
      *            '01' für erster Kontoinhaber
           05  KSDS-NNAME                PIC  X(035).
      *                NACHNAME DES KONTOINHABERS
           05  KSDS-VNAME                PIC  X(035).
      *                VORNAME DES KONTOINHABERS
           05  KSDS-GEBDAT               PIC  9(008).
      *                GEBURTSTAGSDATUM    JJJJMMTT
           05  KSDS-GESCHLECHT           PIC  X(001).
      *                0   KEINE EINMELDUNG VORHANDEN
      *                1   MAENNLICH
      *                2   WEIBLICH
           05  KSDS-ANREDE               PIC  X(001).
      *                ANREDE...........*1 : HERRN
      *                ANREDE...........*2 : FRAU
      *                ANREDE...........*3 : FRAEULEIN ??
      *                ANREDE...........*4 : HERRN und FRAU ??
      *                ANREDE...........*5 : HERREN ??
      *                ANREDE...........*6:  AN ??
      *                ANREDE...........*0 : KEINE ANREDE

            05  KSDS-STR                 PIC  X(028).
      *                STRASSE
            05  KSDS-HAUSNR              PIC  X(009).
      *                Haus-Nummer
            05  KSDS-POSTFACH            PIC  X(009).
      *                POSTFACH
            05  KSDS-PLZ                 PIC  X(009).
      *                POSTLEITZAHL
            05  KSDS-ORT                 PIC  X(028).
      *                ORT
            05  KSDS-AUSL-KZ             PIC  X(003).
      *                AUSLANDSKENNZEICHEN
            05  KSDS-VORWAHL-TEL         PIC  9(006).
      *                Vorwahl zur TELEFON-NR.
            05  KSDS-TEL                 PIC  9(009).

            05  KSDS-DEPOT-AKTIV         PIC  9(001).
...   *              Kennz. = 1, Kunde hat aktives Depot
...   *              Kennz. = 0, Kunde hat kein aktives Depot
      *              Kennz. = 9, Kennz. ist noch nicht geliefert worden

            05  KSDS-BLZ                 PIC  9(008).
      *              BLZ des Kundenkontos

            05  KSDS-DEL-KZ              PIC  X(001).
      *              Kennz. = D, Vertrag gelöscht

           05  KSDS-BIC                       PIC X(011).
      *        BIC des Belastungskontos zur Lastschrift
 
           05  KSDS-ESAFE                PIC  X(001).
      *            LEER
            05  KSDS-RESERVE             PIC  X(046).
      *     05  KSDS-RESERVE             PIC  X(058).
      *     05  KSDS-RESERVE             PIC  X(059).
      *     05  KSDS-RESERVE             PIC  X(068).
      *            RESERVE



