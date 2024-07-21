      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2288.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2288.CPv  $
      *
      *   Rev 2.5   Mar 27 2017 09:28:26   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.4   Aug 30 2011 12:48:52   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.3   Jun 18 2009 10:35:50   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.2   Apr 03 2009 14:22:06   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   Apr 01 2009 14:04:32   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 04 2009 16:55:24   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - EINGABEDATEI SANTANDER DTA AENDERUNGS- / KOMPLETTBE-     *
      *       STAND KORREKTUREN MIT VOR- UND NACHSATZ.                 *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *05.03.09! NEU                                  ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11!Umstellung auf Neutral                ! SWZ     !2.2   *
      *--------!--------------------------------------!---------!----- *
      *24.03.17!FVD entfernt                          ! HHU     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
 
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 01                   *
      *                                                                *
      *     LAENGE:       300 BYTES               STAND: 05.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
110811*01 DRKI2288.
110811 01 'XX'-SATZ.
 
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-V-SA                          PIC X(002).
      *       SATZART = '01'
            05  'XX'-DTA-V-ABSENDER                    PIC X(008).
      *       ABSENDER = 'SEB BANK'
            05  'XX'-DTA-V-ERSTELLDATUM                PIC 9(008).
      *       Buchungsdatum = 'JJJJMMTT'
            05  'XX'-DTA-V-EMPFAENGER                  PIC X(004).
      *       EMPFAENGER = 'DVAG' 'ALLF'
            05  'XX'-DTA-V-TEXT                        PIC X(008).
      *       INHALTSANGABE:
      *       KUNDENK   Datei Kundenstammsatz
      *       KUNDENKW  Datei Kundenstammsatz für weitere
      *                 Kontoinhaber bei Gemeinschaftskonten
      *       KONTOK    Datei Kontokorrentdatensatz
      *       KARTEK    Datei Servicekartendatensatz
      *       KKARTEK   Datei Kreditkartendatensatz
      *       BAUFIK    Datei Baufinanzierungsdatensatz
      *       PKK       Datei Privatkredit-Datensatz
      *
            05  'XX'-DTA-V-WKZ                         PIC X(003).
      *       WÄHRUNGSKENNZEICHEN = 'EUR'
            05  'XX'-DTA-V-FILLER                      PIC X(267).
      *       RESERVE
      *
      **************************************************************** *
      *                                                                *
110811*           Ges. 66 -  Kontokorrentdatensatz KONTOK              *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 05.03.2009      *
      *                                                                *
      **************************************************************** *
          03 'XX'-DTA-KONTOK REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-SA                 PIC  X(002).
      *       Satzart             = '04'
            05  'XX'-DTA-GES                PIC  9(002).
      *       Gesellschafts-Nr.   = '66'
            05  'XX'-DTA-VTRGNR             PIC  9(015).
      *       VertragsNr. = '0000' fix
      *                   = Stellen 1-4 der BLZ
      *                   = Stellen 1-7 der StammNr.
            05  'XX'-DTA-BLZ                PIC  9(008).
      *       BLZ des Girokontos.
            05  'XX'-DTA-KONTONR            PIC  9(010).
      *       Kontonummer des Girokontos.
      *       Kontoart              Stelle 1
      *       Stammnummer           Stelle 2-8
      *       Anzahl der Konten     Stelle 9-10
            05  'XX'-DTA-BEGINN             PIC  9(008).
      *       Datum der Produkteröffnung i.d.F. JHJJMMTT
            05  'XX'-DTA-ENDE               PIC  9(008).
      *       Datum der Produktschliessung i.d.F. JHJJMMTT
            05  'XX'-DTA-ANLAGEART          PIC  9(004).
      *       Kürzel z. Bsp. für 'GIROSTAR', 'Konto for Free' etc.
      *       G66 verschluesselt jede einzelne Anlageart in einem
      *  ,    4-stelligen Feld und teilt der DVAG in einer Tabelle mit
      *       was diese in Klartext bedeuten (Umsetztabelle).
            05  'XX'-DTA-DISPOLIMIT         PIC   9(012)V99.
      *
            05  'XX'-DTA-SALDO-EURO PIC S9(11)V99 SIGN LEADING SEPARATE.
 
      *       incl. Vorzeichen
            05  'XX'-DTA-HABENUMS-KUM       PIC   9(012)V99.
      *       Kumulierte Habenumsaetze des aktuellen Monats
      *       inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *       positiven Kontokorrentabschlusses.
 
            05  'XX'-DTA-SOLLUMS-KUM        PIC   9(012)V99.
      *       Kumulierte Sollumsaetze des aktuellen Monats
      *       inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *       negativen Kontokorrentabschlusses.
 
            05  'XX'-DTA-HABENUMS-KUM-VORM  PIC   9(012)V99.
      *       Kumulierte Habenumsaetze des Vormonats inkl. evtl. in
      *       diesem Monat verbuchter Gesamtsumme eines
      *       positiven Kontokorrentabschlusses.
 
            05  'XX'-DTA-SOLLUMS-KUM-VORM   PIC   9(012)V99.
      *       Kumulierte Sollumsaetze des Vormonats inkl. evtl. in
      *       diesem Monat verbuchter Gesamtsumme eines
      *       negativen Kontokorrentabschlusses.
 
            05  'XX'-DTA-AKT-MONAT          PIC  X(005).
      *       Aktueller Monat, falls Umsaetze geliefert werden.
      *       I.d.F. MM/JJ
            05  'XX'-DTA-VOR-MONAT          PIC  X(005).
      *       Vormonat, falls Umsaetze geliefert werden.
      *       I.d.F. MM/JJ
 
            05  'XX'-DTA-SOLLZINSSATZ       PIC  9(002)V9999.
      *
            05  'XX'-DTA-DISPOZINSSATZ      PIC  9(002)V9999.
      *       Ueberziehungszinssatz
      *
            05  'XX'-DTA-HABENZINSSATZ      PIC  9(002)V9999.
      *
            05  'XX'-DTA-DEL-KZ             PIC  X(001).
      *       DEL-Kennzeichen  ('D' oder Blank)
            05  'XX'-DTA-RESERVE            PIC  X(130).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 01                  *
      *                                                                *
      *     LAENGE:       300 BYTES               STAND: 05.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
      **** NACHSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 'XX'-DTA-NACHSATZ REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-N-SA                    PIC X(002).
      *       SATZART = '99'
            05  'XX'-DTA-N-ANZAHL-RECORD         PIC 9(007).
      *       Hier wird die Anzahl der Datensätze dieser Datei
      *       eingestellt.
      *       Die Anzahl enthält nicht den Vorsatz und auch nicht
      *       diesen Nachsatz (nur die reinen Datensätze)
            05  'XX'-DTA-N-FILLER                PIC X(291).
      *       RESERVE
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2288                       *
      ******************************************************************

