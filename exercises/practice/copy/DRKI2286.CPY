      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2286.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRKI2286.CPv  $
      *
      *   Rev 2.4   Mar 27 2017 09:28:20   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.3   Aug 30 2011 12:48:36   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Apr 03 2009 14:21:56   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   Apr 01 2009 14:04:20   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 04 2009 16:55:02   t13ex52
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - EINGABEDATEI G66 DTA AENDERUNGS- / KOMPLETTBE-           *
      *       STAND KORREKTUREN MIT VOR- UND NACHSATZ.                 *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *04.03.09! NEU                                  ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11!Umstellung auf Neutral                ! SWZ     !2.2   *
      *--------------------------------------------------------------- *
      *24.03.17!FVD entfernt                          ! HHU     !      *
      *--------------------------------------------------------------- *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 01                   *
      *                                                                *
      *     LAENGE:       300 BYTES               STAND: 04.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
       01 'XX'-SATZ.
 
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-V-SA                          PIC X(002).
      *         SATZART = '01'
            05  'XX'-DTA-V-ABSENDER                    PIC X(008).
      *         ABSENDER = 'SEB BANK'
            05  'XX'-DTA-V-ERSTELLDATUM                PIC 9(008).
      *         Buchungsdatum = 'JJJMMTT'
            05  'XX'-DTA-V-EMPFAENGER                  PIC X(004).
      *         EMPFAENGER = 'DVAG' 'ALLF'
            05  'XX'-DTA-V-TEXT                        PIC X(008).
      *         INHALTSANGABE:
      *         KUNDENK   Datei Kundenstammsatz
      *         KUNDENKW  Datei Kundenstammsatz für weitere
      *                   Kontoinhaber bei Gemeinschaftskonten
      *         KONTOK    Datei Kontokorrentdatensatz
      *         KARTEK    Datei Servicekartendatensatz
      *         KKARTEK   Datei Kreditkartendatensatz
      *         BAUFIK    Datei Baufinanzierungsdatensatz
      *         PKK       Datei Privatkredit-Datensatz
      *
            05  'XX'-DTA-V-WKZ                         PIC X(003).
      *         WÄHRUNGSKENNZEICHEN = 'EUR'
            05  'XX'-DTA-V-FILLER                      PIC X(267).
      *         RESERVE
      *
      **************************************************************** *
      *                                                                *
      *             Ges. 66 -  Kundenstammsatz KUNDENK                 *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 04.03.2009      *
      *                                                                *
      **************************************************************** *
      *
          03 'XX'-DTA-KUNDENK REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-SA                 PIC  X(002).
      *         Satzart             = '02'
            05  'XX'-DTA-GES                PIC  9(002).
      *         Gesellschafts-Nr. = 66
            05  'XX'-DTA-VTRGNR             PIC  9(015).
      *         VertragsNr. = '0000' fix
      *                     = Stellen 1-4 der BLZ
      *                     = Stellen 1-7 der StammNr.
            05  'XX'-DTA-VBNR               PIC  9(007).
      *         DVAG-ABSCHLUSSVERMITTLER
            05  'XX'-DTA-VBEGINN            PIC  9(008).
      *         DATUM VERTRAGSBEGINN/Kundeneröffnung i.d.F. JHJJMMTT
            05  'XX'-DTA-VABLAUF            PIC  9(008).
      *         DATUM VERTRAGSABLAUF/Kundenschließung i.d.F. JHJJMMTT
            05  'XX'-DTA-INFOSPERRE-KZ      PIC  X(001).
      *         S = Datenschutzsperre gesetzt
            05  'XX'-DTA-VERFUEG-KZ         PIC  X(001).
      *         KENNZEICHEN VERFUEGUNG
      *         O = Einzelverfügung
      *         U = Gemeinschaftsverfügung
            05  'XX'-DTA-RECORD-LFDNR       PIC  9(002).
      *         Laufende Nummer des Satzes.
      *         01 = erster Kontoinhaber
            05  'XX'-DTA-NNAME              PIC  X(035).
      *         NACHNAME DES KONTOINHABERS.
      *         Kann fuer Vertraege mit Datenschutzsperre leer sein.
            05  'XX'-DTA-VNAME              PIC  X(035).
      *         VORNAME DES KONTOINHABERS. Kann fuer Firmen und fuer
      *         Vertraege mit Datenschutzsperre leer sein.
            05  'XX'-DTA-GEBDAT             PIC  9(008).
      *         GEBURTSTAGSDATUM i.d.F JHJJMMTT
      *         Kann fuer Firmen und fuer Firmen und fuer Vertraege
      *         mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-GESCHLECHT         PIC  9(001).
      *         0   FIRMA
      *         1   MAENNLICH
      *         2   WEIBLICH
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-ANREDE             PIC  9(001).
      *         1   HERR
      *         2   FRAU
      *         3   FIRMA
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-STR-HAUSNR         PIC  X(028).
      *         Strasse und Hausnummer aus Anschrift.
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-PLZ                PIC  X(009).
      *         POSTLEITZAHL aus Anschrift
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-ORT                PIC  X(028).
      *         ORT aus Anschrift
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-LAENDER-KZ         PIC  X(002).
      *         ISO-Code Laenderkennzeichen
      *         Kann fuer  Vertraege mit Datenschutzsperre leer sein.
      *
            05  'XX'-DTA-DEL-KZ             PIC  X(001).
      *         DEL-Kennzeichen
            05  'XX'-DTA-RESERVE            PIC  X(106).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 01                  *
      *                                                                *
      *     LAENGE:       300 BYTES               STAND: 04.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
      **** NACHSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 'XX'-DTA-NACHSATZ REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-N-SA                    PIC X(002).
      *         SATZART = '99'
            05  'XX'-DTA-N-ANZAHL-RECORD         PIC 9(007).
      *         Hier wird die Anzahl der Datensätze dieser Datei
      *         eingestellt.
      *         Die Anzahl enthält nicht den Vorsatz und auch nicht
      *         diesen Nachsatz (nur die reinen Datensätze)
            05  'XX'-DTA-N-FILLER                PIC X(291).
      *         RESERVE
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2286                       *
      ******************************************************************

