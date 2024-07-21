      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2295.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2295.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:49:26   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jun 18 2009 10:36:28   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:12   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 06 2009 11:41:42   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM Santander               *
      *     - Ein- / AusgabeDatei KSDS Komplettbestand                 *
      *       Kontokorrent-Datensatz                                   *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *09.03.09! NEU                                  ! HSL     !      *
      *----------------------------------------------------------------*
      *31.03.09! Zusaetzlichen Key integriert.        ! HSL     !      *
      *----------------------------------------------------------------*
      *11.08.11! Umstellung auf Neutral               ! SWZ     !      *
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
110811*            Ges. 66 -  Kontokorrentdatensatz KONTOK             *
      *                  KSDS - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 09.03.2009      *
      *                                                                *
      **************************************************************** *
       01 'XX'-KSDS-KONTOK.
      *
310309   05 'XX'-KSDS-KONTOK-KEY.
    >>     10 'XX'-KSDS-VKEY-VTRGNR        PIC 9(015).
310309     10 'XX'-KSDS-VKEY-ANZ-KONTEN    PIC 9(004).
      *
         05  'XX'-KSDS-KONTOK-DATEN.
           10  'XX'-KSDS-SA                 PIC  X(002).
      *      Satzart             = '04'
           10  'XX'-KSDS-GES                PIC  9(002).
      *      Gesellschafts-Nr.   = '66'
      ***** VertragsNr. = Schluessel in der KSDS-Datei.
           10  'XX'-KSDS-VTRGNR             PIC  9(015).
      *      VertragsNr. = '0000' fix
      *                  = Stellen 1-4 der BLZ
      *                  = Stellen 1-7 der StammNr.
           10  'XX'-KSDS-BLZ                PIC  9(008).
      *      BLZ des Girokontos.
           10  'XX'-KSDS-KONTONR            PIC  9(010).
      *      Kontonummer des Girokontos.
      *      Kontoart              Stelle 1
      *      Stammnummer           Stelle 2-8
      *      Anzahl der Konten     Stelle 9-10
           10  'XX'-KSDS-BEGINN.
      *      Datum der Produkteröffnung i.d.F. JHJJMMTT
             15 KSDS-JAHR-BEGINN            PIC  9(004).
             15 KSDS-MONAT-BEGINN           PIC  9(002).
             15 KSDS-TAG-BEGINN             PIC  9(002).
      *
           10  'XX'-KSDS-ENDE.
      *      Datum der Produktschliessung i.d.F. JHJJMMTT
             15 KSDS-JAHR-ENDE              PIC  9(004).
             15 KSDS-MONAT-ENDE             PIC  9(002).
             15 KSDS-TAG-ENDE               PIC  9(002).
      *
           10  'XX'-KSDS-ANLAGEART          PIC  9(004).
      *      Kürzel z. Bsp. für 'GIROSTAR', 'Konto for Free' etc.
      *      G66 verschluesselt jede einzelne Anlageart in einem
      *      4-stelligen Feld und teilt der DVAG in einer Tabelle mit,
      *      was diese in Klartext bedeuten (Umsetztabelle).
           10  'XX'-KSDS-DISPOLIMIT         PIC   9(012)V99.
      *
           10  'XX'-KSDS-SALDO-EURO PIC S9(11)V99 SIGN LEADING SEPARATE.
      *      Strasse aus Anschrift
           10  'XX'-KSDS-HABENUMS-KUM       PIC  9(012)V99.
      *      Kumulierte Habenumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.
           10  'XX'-KSDS-SOLLUMS-KUM        PIC   9(012)V99.
      *      Kumulierte Sollumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.
           10  'XX'-KSDS-HABENUMS-KUM-VORM  PIC   9(012)V99.
      *      Kumulierte Habenumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.
           10  'XX'-KSDS-SOLLUMS-KUM-VORM   PIC   9(012)V99.
      *      Kumulierte Sollumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.
           10  'XX'-KSDS-AKT-MONAT          PIC  X(005).
      *      Aktueller Monat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
           10  'XX'-KSDS-VOR-MONAT          PIC  X(005).
      *      Vormonat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
           10  'XX'-KSDS-SOLLZINSSATZ       PIC  9(002)V9999.
      *
           10  'XX'-KSDS-DISPOZINSSATZ      PIC  9(002)V9999.
      *      Ueberziehungszinssatz
      *
           10  'XX'-KSDS-HABENZINSSATZ      PIC  9(002)V9999.
      *
           10  'XX'-KSDS-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen  ('D' oder Blank)
           10  'XX'-KSDS-RESERVE            PIC  X(111).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2295                       *
      ******************************************************************
