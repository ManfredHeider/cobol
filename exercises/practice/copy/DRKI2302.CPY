      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2302.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2302.CPv  $
      *
      *   Rev 2.3   Aug 30 2011 12:49:50   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.2   Jun 18 2009 10:36:42   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   May 22 2009 17:36:28   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 12 2009 17:09:46   t13ex52
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *    - Zwischenbereich DTA fuer die Aufbereitung des KSDS-Satzes *
      *      Kontokorrent-Datensatz                                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.03.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *02.04.09! Zusaetzlichen Key integriert.        ! HSL     !      *
      *        !                                      !         !      *
      *11.08.11! Umstellung auf Neutral               ! SWZ     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      **************************************************************** *
      *                                                                *
      *    Santander   - Bank Kontokorrentdatensatz KONTOK             *
      *                  KSDS - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   300 BYTES                 STAND: 18.03.2009      *
      *                                                                *
      **************************************************************** *
       01 'XX'-ZWSP-KONTOK.
      *
020409   05 'XX'-ZWSP-KONTOK-KEY.
    >>     10 'XX'-ZWSP-KEY-VTRGNR        PIC 9(015).
020409     10 'XX'-ZWSP-KEY-ANZ-KONTEN    PIC 9(004).
      *
         05  'XX'-ZWSP-KONTOK-DATEN.
      *
           10  'XX'-ZWSP-SA                 PIC  X(002).
      *      Satzart             = '04'
           10  'XX'-ZWSP-GES                PIC  9(002).
      *      Gesellschafts-Nr.   = '66'
      ***** VertragsNr. = Schluessel in der KSDS-Datei.
           10  'XX'-ZWSP-VTRGNR             PIC  9(015).
      *      VertragsNr. = '0000' fix
      *                  = Stellen 1-4 der BLZ
      *                  = Stellen 1-7 der StammNr.
           10  'XX'-ZWSP-BLZ                PIC  9(008).
      *      BLZ des Girokontos.
           10  'XX'-ZWSP-KONTONR            PIC  9(010).
      *      Kontonummer des Girokontos.
      *      Kontoart              Stelle 1
      *      Stammnummer           Stelle 2-8
      *      Anzahl der Konten     Stelle 9-10
           10  'XX'-ZWSP-BEGINN.
      *      Datum der Produkteröffnung i.d.F. JHJJMMTT
             15 ZWSP-JAHR-BEGINN            PIC  9(004).
             15 ZWSP-MONAT-BEGINN           PIC  9(002).
             15 ZWSP-TAG-BEGINN             PIC  9(002).
      *
           10  'XX'-ZWSP-ENDE.
      *      Datum der Produktschliessung i.d.F. JHJJMMTT
             15 ZWSP-JAHR-ENDE              PIC  9(004).
             15 ZWSP-MONAT-ENDE             PIC  9(002).
             15 ZWSP-TAG-ENDE               PIC  9(002).
      *
           10  'XX'-ZWSP-ANLAGEART          PIC  9(004).
      *      Kürzel z. Bsp. für 'GIROSTAR', 'Konto for Free' etc.
      *      G66 verschluesselt jede einzelne Anlageart in einem
      *      4-stelligen Feld und teilt der DVAG in einer Tabelle mit,
      *      was diese in Klartext bedeuten (Umsetztabelle).
           10  'XX'-ZWSP-DISPOLIMIT         PIC   9(012)V99.
      *
           10  'XX'-ZWSP-SALDO-EURO         PIC  S9(012)V99.
      *      Strasse aus Anschrift
           10  'XX'-ZWSP-HABENUMS-KUM       PIC  S9(012)V99.
      *      Kumulierte Habenumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.

           10  'XX'-ZWSP-SOLLUMS-KUM        PIC  S9(012)V99.
      *      Kumulierte Sollumsaetze des aktuellen Monats
      *      inkl. evtl. in diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.

           10  'XX'-ZWSP-HABENUMS-KUM-VORM  PIC  S9(012)V99.
      *      Kumulierte Habenumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      positiven Kontokorrentabschlusses.

           10  'XX'-ZWSP-SOLLUMS-KUM-VORM   PIC  S9(012)V99.
      *      Kumulierte Sollumsaetze des Vormonats inkl. evtl. in
      *      diesem Monat verbuchter Gesamtsumme eines
      *      negativen Kontokorrentabschlusses.

           10  'XX'-ZWSP-AKT-MONAT          PIC  X(005).
      *      Aktueller Monat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
           10  'XX'-ZWSP-VOR-MONAT          PIC  X(005).
      *      Vormonat, falls Umsaetze geliefert werden.
      *      I.d.F. MM/JJ
           10  'XX'-ZWSP-SOLLZINSSATZ       PIC  9(002)V9999.
      *
           10  'XX'-ZWSP-DISPOZINSSATZ      PIC  9(002)V9999.
      *      Ueberziehungszinssatz
      *
           10  'XX'-ZWSP-HABENZINSSATZ      PIC  9(002)V9999.
      *
           10  'XX'-ZWSP-DEL-KZ             PIC  X(001).
      *      DEL-Kennzeichen  ('D' oder Blank)
           10  'XX'-ZWSP-RESERVE            PIC  X(113).
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2302                       *
      ******************************************************************
