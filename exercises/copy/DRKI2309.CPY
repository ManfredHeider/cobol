      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  18.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2309.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2309.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:50:22   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jun 18 2009 10:36:56   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:46   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 18 2009 10:23:08   t13ex52
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
      *--------!--------------------------------------!---------!----- *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !2.3   *
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
      *            G66 - Bank Kontokorrentdatensatz KONTOK             *
      *                 A U S G A B E  -  S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
110811*01 'XX'-KONTOK-AENDERUNG.
110811 01 'XX'-SATZ.
      *
         05 'XX'-VERARB-KNZ          PIC X(001).
      *     Verarbeitungskennzeichen
      *     I = Insert U = Update D = Delete
      *
         05 'XX'-KONTOK-KEY.
           10 'XX'-VKEY-VTRGNR        PIC 9(015).
           10 'XX'-VKEY-ANZ-KONTEN    PIC 9(004).
      *
         05  'XX'-SA                 PIC  X(002).
      *      Satzart             = '04'
         05  'XX'-GES                PIC  9(002).
      *      Gesellschafts-Nr.   = '66'
         05  'XX'-VTRGNR             PIC  9(015).
      *      VertragsNr. = '0000' fix
      *                  = Stellen 1-4 der BLZ
      *                  = Stellen 1-7 der StammNr.
         05  'XX'-BLZ                PIC  9(008).
      *      BLZ des Girokontos.
         05  'XX'-BLZ-UPD            PIC  X.
      *      'J' = Aenderung BLZ des Girokontos.

         05  'XX'-KONTONR            PIC  9(010).
      *      Kontonummer des Girokontos.
      *      Kontoart              Stelle 1
      *      Stammnummer           Stelle 2-8
      *      Anzahl der Konten     Stelle 9-10
         05  'XX'-KONTONR-UPD        PIC  X.
      *      'J' = Aenderung Kontonummer des Girokontos.

         05  'XX'-BEGINN             PIC  9(008).
      *      Datum der Produkteröffnung i.d.F. JHJJMMTT
         05  'XX'-BEGINN-UPD         PIC  X.
      *      'J' = Aenderung Datum der Produkteröffnung

         05  'XX'-ENDE               PIC  9(008).
      *      Datum der Produktschliessung i.d.F. JHJJMMTT
         05  'XX'-ENDE-UPD           PIC  X.
      *      'J' = Aenderung Datum der Produktschliessung

         05  'XX'-ANLAGEART          PIC  9(004).
      *      Kürzel z. Bsp. für 'GIROSTAR', 'Konto for Free' etc.
      *      G66 verschluesselt jede einzelne Anlageart in einem
      *      4-stelligen Feld und teilt der DVAG in einer Tabelle mit,
      *      was diese in Klartext bedeuten (Umsetztabelle).
         05  'XX'-ANLAGEART-UPD      PIC  X.
      *      'J' = Aenderung ANLAGEART

         05  'XX'-DISPOLIMIT         PIC   9(012)V99.
      *
         05  'XX'-DISPOLIMIT-UPD     PIC   X.
      *      'J' = Aenderung DISPOLIMIT

         05  'XX'-SALDO-EURO         PIC  S9(012)V99.
      *      incl. Vorzeichen
         05  'XX'-SALDO-EURO-UPD     PIC  X.
      *      'J' = Aenderung Saldo-Euro

         05  'XX'-HABENUMS-KUM       PIC  9(012)V99.
      *      Kumulierte Habenumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.
         05  'XX'-HABENUMS-KUM-UPD   PIC  X.
      *      'J' = Aenderung Habenums-Kum

         05  'XX'-SOLLUMS-KUM        PIC  9(012)V99.
      *      Kumulierte Sollumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.
         05  'XX'-SOLLUMS-KUM-UPD    PIC  X.
      *      'J' = Aenderung SOLLUMS-KUM

         05  'XX'-HABENUMS-KUM-VORM  PIC  9(012)V99.
      *      Kumulierte Habenumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.
         05  'XX'-HABENUMS-KUM-VORM-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-HABENUMS-KUM-VORM

         05  'XX'-SOLLUMS-KUM-VORM   PIC  9(012)V99.
      *      Kumulierte Sollumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.
         05  'XX'-SOLLUMS-KUM-VORM-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-SOLLUMS-KUM-VORM

         05  'XX'-AKT-MONAT          PIC  X(005).
      *      Aktueller Monat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
         05  'XX'-AKT-MONAT-UPD      PIC  X.
      *      'J' = Aenderung  'XX'-AKT-MONAT

         05  'XX'-VOR-MONAT          PIC  X(005).
      *      Vormonat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
         05  'XX'-VOR-MONAT-UPD      PIC  X.
      *      'J' = Aenderung  'XX'-VOR-MONAT

         05  'XX'-SOLLZINSSATZ       PIC  9(002)V9999.
      *
         05  'XX'-SOLLZINSSATZ-UPD   PIC  X.
      *      'J' = Aenderung 'XX'-SOLLZINSSATZ

         05  'XX'-DISPOZINSSATZ      PIC  9(002)V9999.
      *      Ueberziehungszinssatz
         05  'XX'-DISPOZINSSATZ-UPD  PIC  X.
      *      'J' = Aenderung Ueberziehungszinssatz
      *
         05  'XX'-HABENZINSSATZ      PIC  9(002)V9999.
      *
         05  'XX'-HABENZINSSATZ-UPD  PIC  X.
      *      'J' = Aenderung 'XX'-HABENZINSSATZ

         05  'XX'-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen  ('D' oder Blank)
         05  'XX'-DEL-KZ-UPD         PIC  X.
      *      'J= = Aenderung DEL-Kennzeichen in 'D' oder Blank

         05  'XX'-RESERVE            PIC  X(93).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2309                       *
      ******************************************************************
