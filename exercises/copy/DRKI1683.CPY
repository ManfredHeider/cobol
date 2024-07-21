      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.06.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI1683.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI1683.CPv  $
      *
      *   Rev 2.4   Sep 02 2013 12:15:08   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.3   Sep 02 2013 09:57:50   t13ex14
      *AKI01204: SEPA - DB-Online - Deuba Kundenstammsatz
      *
      *   Rev 2.2   Mar 16 2005 11:45:58   t13ex39
      *FKI00922: DeuBa Hist. KSDS Tagesdatum
      *
      *   Rev 2.1   Jul 05 2004 10:33:58   t13ex39
      *AKI00082: DEUTSCHE BANK
      *
      *   Rev 2.0   Jun 17 2004 10:37:18   t13ex39
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ******************************************************************
      *                                                                *
      *      COPY-MEMBER für KSDS KUNDEN-/VERTRAGSDATEN VON DB Historie*
      *                                                                *
      *    SATZLÄNGE   302                                             *
      *                                                                *
      *    LOGBUCH                                                     *
      *--------!-------------------------------------------!-----------*
      *AUFTRAG !Datum   !Text                              !Name       *
      *--------!-------------------------------------------!-----------*
      *AKI00082!11.06.04! Neuerstellung                    !WHO        *
      *        !        !                                  !           *
      *--------!-------------------------------------------!-----------*
      *FKI00992!10.03.05! Tagesdatum der Löschung          !WHO        *
      *----------------------------------------------------------------*
240713*2AKI01204!24.07.13! NEUE FELDER EINGEFÜGT f. SEPA   !WMI        *
240713*         !        !   UND DEN FILLER ANGEPASST      !           *
      *        !        !                                  !           *
      *        !        !                                  !           *
      *        !        !                                  !           *
      * ---------------------------------------------------------------*
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
CCC009*    SKIP1
       01  DRKI1683.

           05  KSDS-SA                   PIC  X(001).
      *            SATZART        = '2'
           05  KSDS-GES                  PIC  9(002).
      *            GESELLSCHAFT   = '48'
           05  KSDS-VNR.
               10 FILIALNUMMER           PIC  9(003).
               10 KUNDENNUMMER           PIC  9(007).
      *            VERTRAGSNUMMER MIT FÜHRENDEN NULLEN (DEPOTNUMMER)
               10 KSDS-LFDNR             PIC  9(002).
      *            LFD. NR. FÜR PRIMARY-KEY
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

100305      05  KSDS-DEL-KZ              PIC  X(001).
100305*              Kennz. = D, Vertrag gelöscht

100305      05  KSDS-AEND-DAT            PIC  X(10).
100305*         ÄNDERUNGSDATUM
240713      05  KSDS-BIC                 PIC X(011).
240713*        BIC des Belastungskontos zur Lastschrift
240713
240713      05  KSDS-RESERVE             PIC  X(037).
240713*     05  KSDS-RESERVE             PIC  X(048).
100305*     05  KSDS-RESERVE             PIC  X(059).
      *     05  KSDS-RESERVE             PIC  X(068).
      *            RESERVE


