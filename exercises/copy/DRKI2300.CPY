      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2300.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2300.CPv  $
      *
      *   Rev 2.3   Mar 27 2017 09:28:44   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.2   Aug 30 2011 12:49:44   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.1   May 22 2009 17:36:24   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 12 2009 17:09:28   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - Zwischenbereich DTA fuer die Aufbereitung des KSDS-      *
      *       Datensatzes.                                             *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.03.09! NEU                                  ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !      *
      *--------!--------------------------------------!---------!----- *
      *24.03.17! FVD entfernt                         ! HHU     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *        Santander- Bank Kundenstammsatz KUNDENK                 *
      *                 ZWSP - D A T E N S A T Z                       *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   309 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
       01 'XX'-ZWSP-KUNDENK.
      *
         05 'XX'-ZWSP-KUNDENK-DATEN.
           10 'XX'-ZWSP-SA                 PIC  X(002).
      *       Satzart           = 02
           10 'XX'-ZWSP-GES                PIC  9(002).
      *       Gesellschafts-Nr. = 66
           10 'XX'-ZWSP-VTRGNR             PIC  9(015).
      *       VertragsNr. = '0000' fix
      *                   = Stellen 1-4 der BLZ
      *                   = Stellen 1-7 der StammNr.
           10 'XX'-ZWSP-VBNR               PIC  9(007).
      *       DVAG-ABSCHLUSSVERMITTLER
           10 'XX'-ZWSP-VBEGINN            PIC  9(008).
      *       DATUM VERTRAGSBEGINN/Kundeneroeffnung i.d.F. JHJJMMTT
           10 'XX'-ZWSP-VABLAUF            PIC  9(008).
      *       DATUM VERTRAGSABLAUF/Kundenschliessung i.d.F. JHJJMMTT
           10 'XX'-ZWSP-INFOSPERRE-KZ      PIC  X(001).
      *       S = Datenschutzsperre gesetzt
           10 'XX'-ZWSP-VERFUEG-KZ         PIC  X(001).
      *       KENNZEICHEN VERFUEGUNG
      *       O = Einzelverfügung
      *       U = Gemeinschaftsverfügung
           10 'XX'-ZWSP-RECORD-LFDNR       PIC  9(002).
      *       Laufende Nummer des Satzes.
      *       01 = erster Kontoinhaber
           10 'XX'-ZWSP-NNAME              PIC  X(035).
      *       NACHNAME DES KONTOINHABERS. Kann fuer Vertraege mit Daten-
      *       schutzsperre leer sein.
           10 'XX'-ZWSP-VNAME              PIC  X(035).
      *       VORNAME DES KONTOINHABERS. Kann fuer Firmen und fuer
      *       Vertraege mit Datenschutzsperre leer sein.
           10 'XX'-ZWSP-GEBDAT             PIC  9(008).
      *       GEBURTSTAGSDATUM i.d.F JHJJMMTT
      *       Kann fuer Firmen und fuer Firmen und fuer Vertraege
      *       mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-GESCHLECHT         PIC  9(001).
      *       0   FIRMA
      *       1   MAENNLICH
      *       2   WEIBLICH
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-ANREDE             PIC  9(001).
      *       1   HERR
      *       2   FRAU
      *       3   FIRMA
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-STR-HAUSNR         PIC  X(028).
      *       Strasse und Hausnummer aus Anschrift.
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-PLZ                PIC  X(009).
      *       POSTLEITZAHL aus Anschrift
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-ORT                PIC  X(028).
      *       ORT aus Anschrift
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-LAENDER-KZ         PIC  X(002).
      *       ISO-Code Laenderkennzeichen
      *       Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
           10 'XX'-ZWSP-DEL-KZ             PIC  X(001).
      *       DEL-Kennzeichen
           10 'XX'-ZWSP-RESERVE            PIC  X(106).
      *
         05 'XX'-ZWSP-KONTOK-MANDANT.
      ****
           10 'XX'-ZWSP-MANDNR             PIC  9(009).
      *      004400001 = DVAG
      *      004400003 = ALLFINANZ
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2300                       *
      ******************************************************************

