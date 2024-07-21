      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2291.CPY  $
      *    $Revision:   2.8  $
      *
      *    $Log:   L:/mf/CPY/DRKI2291.CPv  $
      *
      *   Rev 2.8   Mar 28 2017 07:57:30   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.7   Aug 30 2011 12:49:00   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.6   Oct 29 2010 08:16:36   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB PGM-Änderung wg. Zusä
      *tzliche Felder
      *
      *   Rev 2.5   Aug 13 2010 09:23:48   t13ex15
      *AKI00948: Erweiterung Datenspiegel SEB PGM-Änderung wg. Zusä
      *tzliche Felder
      *
      *   Rev 2.4   Jun 18 2009 10:36:12   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.3   May 22 2009 17:34:48   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.2   Apr 03 2009 14:22:12   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   Apr 01 2009 14:04:40   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 04 2009 16:55:50   t13ex52
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
      *06.03.09! NEU                                  ! HSL     !      *
      *18.05.09! Feld 10 "Aktueller Darlehnsbetrag"   ! HSL     !      *
      *        ! neu mit Vorzeichen                   !         !      *
      *        ! Feld 15 Zahlungsweise neuer Inhalt   !         !      *
      *        ! 9=Forward-Darlehen und 0=geloeschte  !         !      *
      *        ! Konten moeglich.                     !         !      *
      *14.06.10! Feld 19 "Nominalzinssatz"            ! SWZ     !      *
      *        ! Feld 20 "Darlehensart" mit  Inhalt   !         !      *
      *        ! A=KFW-Darlehen, B=Zwischenfinanzier.,!         !      *
      *        ! C=Forward-Darlehen, D=Baudarlehen    !         !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11!Umstellung auf Neutral                ! SWZ     !2.2   *
      *--------!--------------------------------------!---------!----- *
      *28.03.17! FVD entfernt                         ! HHU     !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 01                   *
      *                                                                *
      *     LAENGE:       250 BYTES               STAND: 06.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
110811*01 DRKI2291.
110811 01 'XX'-SATZ.
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
           03 'XX'-DTA-VORSATZ.
      *
             05  'XX'-DTA-V-SA                          PIC X(002).
      *          SATZART = '01'
             05  'XX'-DTA-V-ABSENDER                    PIC X(008).
      *          ABSENDER = 'SEB BANK'
             05  'XX'-DTA-V-ERSTELLDATUM                PIC 9(008).
      *          Buchungsdatum = 'JJJJMMTT'
             05  'XX'-DTA-V-EMPFAENGER                  PIC X(004).
      *          EMPFAENGER = 'DVAG' 'ALLF'
             05  'XX'-DTA-V-TEXT                        PIC X(008).
      *          INHALTSANGABE:
      *          KUNDENK   Datei Kundenstammsatz
      *          KUNDENKW  Datei Kundenstammsatz für weitere
      *                    Kontoinhaber bei Gemeinschaftskonten
      *          KONTOK    Datei Kontokorrentdatensatz
      *          KARTEK    Datei Servicekartendatensatz
      *          KKARTEK   Datei Kreditkartendatensatz
      *          BAUFIK    Datei Baufinanzierungsdatensatz
      *          PKK       Datei Privatkredit-Datensatz
      *
             05  'XX'-DTA-V-WKZ                         PIC X(003).
      *          WÄHRUNGSKENNZEICHEN = 'EUR'
             05  'XX'-DTA-V-FILLER                      PIC X(217).
      *          RESERVE
      *
      **************************************************************** *
      *                                                                *
110811*           Ges. 66 -  Baufinanzierungs-Datensatz BAUFIK         *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   250 BYTES                 STAND: 06.03.2009      *
      *                                                                *
      **************************************************************** *
          03 'XX'-DTA-BAUFIK REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-SA                 PIC  X(002).
      *         Satzart             = '07'
            05  'XX'-DTA-GES                PIC  9(002).
      *         Gesellschafts-Nr.   = '66'
            05  'XX'-DTA-VTRGNR             PIC  9(015).
      *         VertragsNr. = '0000' fix
      *                     = Stellen 1-4 der BLZ
      *                     = Stellen 1-7 der StammNr.
            05  'XX'-DTA-BLZ                PIC  9(008).
      *         BLZ des Girokontos
            05  'XX'-DTA-KONTONR            PIC  9(010).
      *         Kontonummer der Baufinanzierung.
      *         Kontoart              Stelle 1
      *         Stammnummer           Stelle 2-8
      *         Anzahl der Konten     Stelle 9-10
            05  'XX'-DTA-DATUM-EROEFFNUNG   PIC  9(008).
      *         Datum Produkteroeffnung i.d.F. JHJJMMTT
            05  'XX'-DTA-DATUM-SCHLIESSUNG  PIC  9(008).
      *         Datum Produktschliessung i.d.F. JHJJMMTT
            05  'XX'-DTA-DARLEHNSSUMME      PIC  9(012)V99.
      *         Ursprungskreditbetrag
            05  'XX'-DTA-DAT-ENDE-ZINSFEST  PIC  9(008).
      *         Datum Ende der Zinsfestschreibung i.F.JHJJMMTT
            05  'XX'-DTA-AKT-DARLEHNBETRG
                                   PIC S9(11)V99 SIGN LEADING SEPARATE.
      *         aktueller Darlehnsstand
            05  'XX'-DTA-BEGINN-TILGUNG     PIC  9(008).
      *         Ende Zeitraum Freijahre, Beginn der Tilgung
      *         i.d.F. JHJJMMTT
            05  'XX'-DTA-TILGUNGSART        PIC  X(001).
      *         A =Annuitätisch
      *         E=Endfällig
      *         G66 setzt numerischen Wert in 'A' bzw. 'E' um.
            05  'XX'-DTA-START-TILGUNGSSATZ PIC  9(002)V9999.
      *         Anfaengliche Tilgung in Prozent.
140610*     05  'XX'-DTA-SOLL-ZINSSATZ      PIC  9(002)V9999.
            05  'XX'-DTA-EFFEKTIV-ZINSSATZ  PIC  9(002)V9999.
      *         Effektivzins
            05  'XX'-DTA-ZAHLWEISE          PIC  9(001).
      *         1 = Jaehrlich
      *         2 = halbjaehrlich
      *         4 = vierteljaehrlich
      *         5 = zweimonatlich
      *         6 = monatlich
      *         9 = Forward Darlehen (wird in unser System K-ZAHLWEISE-
      *                               UNBEKANNT(-2) uebernommen.)
      *         0 = geloeschte Konten (wird in unser System K-ZAHLWEISE-
      *                                UNBEKANNT(-2) uebernommen.)
            05  'XX'-DTA-RATE-IN-EURO       PIC  9(012)V99.
      *         Annuitaet in Euro
            05  'XX'-DTA-KEY-KZ             PIC  X(003).
      *         fixer Wert + letzte beiden Stellen der Kontonummer
      *         SAP 9 + letzte 2 Stellen  Kontonr.,
      *         GEK 1 + letzte 2 Stellen  Kontonr.
            05  'XX'-DTA-DEL-KZ             PIC  X(001).
      *         DEL-Kennzeichen  ('D' oder Blank)
140610      05  'XX'-DTA-DARLEHENSART       PIC  X(001).
140610*         Baufinanzierungsart
140610*         A = KFW-Darlehen
140610*         B = Zwischenfinanzierung
140610*         C = Forward-Darlehen
140610*         D = Baudarlehen
281010      05  'XX'-NICHT-GENUTZT          PIC  X(019).
281010*         19 Stellen ungenutzt, aber mit Feldnummer
140610      05  'XX'-DTA-SOLL-ZINSSATZ      PIC  9(002)V9999.
140610*         Sollzins (Nominalzins in %)
140610*     05  'XX'-DTA-RESERVE            PIC  X(121).
140610      05  'XX'-DTA-RESERVE            PIC  X(095).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 01                  *
      *                                                                *
      *     LAENGE:       250 BYTES               STAND: 06.03.2009    *
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
            05  'XX'-DTA-N-FILLER                PIC X(241).
      *         RESERVE
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2291                       *
      ******************************************************************

