      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2307.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2307.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:50:10   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jul 15 2009 09:30:34   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:42   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 18 2009 10:07:52   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - AUSGABEDATEI G66 Aufbau wie DTA-Eingabe, erweitert       *
      *       mit einem Feld fuer das Aenderungskennzeichen je Attribut*
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.03.09! NEU                                  ! HSL     !      *
      *11.07.09! Feld SEB-AUS-LAENDER-KZ von 2 Stellen! HSL     !      *
      *        ! auf 3 Stellen erweitert              !         !      *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !2.3   *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
110811*             Ges. 66 -  Kundenstammsatz KUNDENK                 *
      *                 A U S G A B E  -  S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
110811*01 'XX'-KUNDENK-AENDERUNG.
       01 'XX'-AUS-SATZ.
      *
         05 'XX'-AUS-VERARB-KNZ          PIC X(001).
      *     Verarbeitungskennzeichen
      *     I = Insert U = Update D = Delete
         05  'XX'-AUS-SA                 PIC  X(002).
      *      Satzart             = '02'
         05  'XX'-AUS-GES                PIC  9(002).
      *      Gesellschafts-Nr.   = '66'
         05  'XX'-AUS-VTRGNR             PIC  9(015).
      *      VertragsNr. = '0000' fix
      *                  = Stellen 1-4 der BLZ
      *                  = Stellen 1-7 der StammNr.
         05  'XX'-AUS-VBNR               PIC  9(007).
      *      DVAG-ABSCHLUSSVERMITTLER
         05  'XX'-AUS-VBNR-UPD           PIC  X.
      *      'J' = Aenderung DVAG-ABSCHLUSSVERMITTLER

         05  'XX'-AUS-VBEGINN            PIC  9(008).
      *      DATUM VERTRAGSBEGINN/Kundeneröffnung i.d.F. JHJJMMTT
         05  'XX'-AUS-VBEGINN-UPD        PIC  X.
      *      'J' = Aenderung DATUM VERTRAGSBEGINN/Kundeneröffnung

         05  'XX'-AUS-VABLAUF            PIC  9(008).
      *      DATUM VERTRAGSABLAUF/Kundenschließung i.d.F. JHJJMMTT
         05  'XX'-AUS-VABLAUF-UPD        PIC  X.
      *      'J' = Aenderung DATUM VERTRAGSABLAUF/Kundenschliessung

         05  'XX'-AUS-INFOSPERRE-KZ      PIC  X(001).
      *      S = Datenschutzsperre gesetzt
         05  'XX'-AUS-INFOSPERRE-KZ-UPD  PIC  X.
      *      'J' = Aenderung Datenschutzsperre-Kennzeichen

         05  'XX'-AUS-VERFUEG-KZ         PIC  X(001).
      *      KENNZEICHEN VERFUEGUNG
      *      O = Einzelverfügung
      *      U = Gemeinschaftsverfügung
         05  'XX'-AUS-VERFUEG-KZ-UPD     PIC  X.
      *      'J' = Aenderung KENNZEICHEN VERFUEGUNG

         05  'XX'-AUS-PERSON-VERARB-KNZ    PIC  X.
      *      VERARBEITUNGSKENNZEICHEN
      *      I = INSERT für Person
      *      U = UPDATE für Person

         05  'XX'-AUS-RECORD-LFDNR       PIC  9(002).
      *      Laufende Nummer des Satzes.
      *      01 = erster Kontoinhaber
         05  'XX'-AUS-RECORD-LFDNR-UPD   PIC  X.
      *      'J' = Aenderung Laufende Nummer des Satzes.

         05  'XX'-AUS-NNAME              PIC  X(035).
      *      NACHNAME DES KONTOINHABERS.
      *      Kann fuer Vertraege mit Datenschutzsperre leer sein.
         05  'XX'-AUS-NNAME-UPD          PIC  X.
      *      'J' = Aenderung NACHNAME DES KONTOINHABERS.

         05  'XX'-AUS-VNAME              PIC  X(035).
      *      VORNAME DES KONTOINHABERS. Kann fuer Firmen und fuer
      *      Vertraege mit Datenschutzsperre leer sein.
         05  'XX'-AUS-VNAME-UPD          PIC  X.
      *      'J' = Aenderung VORNAME DES KONTOINHABERS.

         05  'XX'-AUS-GEBDAT             PIC  9(008).
      *      GEBURTSTAGSDATUM i.d.F JHJJMMTT
      *      Kann fuer Firmen und fuer Firmen und fuer Vertraege
      *      mit Datenschutzsperre leer sein.
         05  'XX'-AUS-GEBDAT-UPD         PIC  X.
      *      'J' = Aenderung GEBURTSTAGSDATUM i.d.F JHJJMMTT
      *
         05  'XX'-AUS-GESCHLECHT         PIC  9(001).
      *      0   FIRMA
      *      1   MAENNLICH
      *      2   WEIBLICH
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
         05  'XX'-AUS-GESCHLECHT-UPD     PIC  X.
      *      'J' = Aenderung Geschlecht

         05  'XX'-AUS-ANREDE             PIC  9(001).
      *      1   HERR
      *      2   FRAU
      *      3   FIRMA
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
         05  'XX'-AUS-ANREDE-UPD         PIC  X.
      *      'J' = Aenderung Anrede

         05  'XX'-AUS-STR-HAUSNR         PIC  X(028).
      *      Strasse und Hausnummer aus Anschrift.
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
         05  'XX'-AUS-STR-HAUSNR-UPD     PIC  X.
      *      'J' = Aenderung STR-HAUSNNR

         05  'XX'-AUS-PLZ                PIC  X(009).
      *      POSTLEITZAHL aus Anschrift
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
         05  'XX'-AUS-PLZ-UPD            PIC  X.
      *      'J' = Aenderung POSTLEITZAHL aus Anschrift

         05  'XX'-AUS-ORT                PIC  X(028).
      *      ORT aus Anschrift
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
         05  'XX'-AUS-ORT-UPD            PIC  X.
      *      'J' = Aenderung ORT aus Anschrift

HSL      05  'XX'-AUS-LAENDER-KZ         PIC  X(003).
      *      ISO-Code Laenderkennzeichen
      *      Kann fuer  Vertraege mit Datenschutzsperre leer sein.
         05  'XX'-AUS-LAENDER-KZ-UPD     PIC  X.
      *      'J' = Aenderung ISO-Code Laenderkennzeichen
      *
         05  'XX'-AUS-NNAME-ALT          PIC  X(035).
      *      NACHNAME DES KONTOINHABERS-ALT.
         05  'XX'-AUS-VNAME-ALT          PIC  X(035).
      *      VORNAME DES KONTOINHABERS-ALT.
         05  'XX'-AUS-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen
         05  'XX'-AUS-DEL-KZ-UPD         PIC  X(001).
      *      'J' = Aenderung DEL-Kennzeichen
      *
         05  'XX'-AUS-WKZ                PIC  X(003).
      *      Waehrungskennzeichen aus Vorsatz
      *
         05  'XX'-AUS-LETZT-AENDDAT      PIC  9(008).
      *      Buchungsdatum aus Vorsatz
      *
         05  'XX'-AUS-RESERVE            PIC  X(006).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2307                       *
      ******************************************************************
