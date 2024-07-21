      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  04.03.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2292.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRKI2292.CPv  $
      *
      *   Rev 2.5   Mar 27 2017 09:28:32   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.4   Aug 30 2011 12:49:06   t13ex15
      *AKI01072: Techn. Umbenennung SEB-Bank in Santander
      *
      *   Rev 2.3   Jun 18 2009 10:36:20   t13ex51
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.2   Apr 03 2009 14:22:22   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.1   Apr 01 2009 14:04:48   t13ex52
      *AKI00764: DVAG monatliche Datenlieferung SEB
      *
      *   Rev 2.0   Mar 04 2009 16:55:58   t13ex52
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
      *09.03.09! NEU                                  ! HSL     !      *
      *--------!--------------------------------------!---------!----- *
      *11.08.11!Umstellung auf Neutral                ! SWZ     !2.2   *
      *--------!--------------------------------------!---------!----- *
      *24.03.17!FVD entfernt                          ! HHU     !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 01                   *
      *                                                                *
      *     LAENGE:       200 BYTES               STAND: 09.03.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
110811*01 DRKI2292.
110811 01 'XX'-SATZ.
 
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-V-SA                          PIC X(002).
      *         SATZART = '01'
            05  'XX'-DTA-V-ABSENDER                    PIC X(008).
      *         ABSENDER = 'SEB BANK'
            05  'XX'-DTA-V-ERSTELLDATUM                PIC 9(008).
      *         Buchungsdatum = 'JJJJMMTT'
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
            05  'XX'-DTA-V-FILLER                      PIC X(167).
      *         RESERVE
      *
      **************************************************************** *
      *                                                                *
110811*              Ges. 66 -  Privatkredit-Datensatz PKK             *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 09.03.2009      *
      *                                                                *
      **************************************************************** *
          03 'XX'-DTA-PKK REDEFINES 'XX'-DTA-VORSATZ.
      *
            05  'XX'-DTA-SA                 PIC  X(002).
      *         Satzart             = '08'
            05  'XX'-DTA-GES                PIC  9(002).
      *         Gesellschafts-Nr.   = '66'
            05  'XX'-DTA-VTRGNR             PIC  9(015).
      *         VertragsNr. = '0000' fix
      *                     = Stellen 1-4 der BLZ
      *                     = Stellen 1-7 der StammNr.
            05  'XX'-DTA-BLZ                PIC  9(008).
      *         BLZ des Girokontos
            05  'XX'-DTA-KONTONR            PIC  9(010).
      *         Kontonummer der PK-Darlehens.
      *         Kontoart              Stelle 1
      *         Stammnummer           Stelle 2-8
      *         Anzahl der Konten     Stelle 9-10
            05  'XX'-DTA-DATUM-EROEFFNUNG   PIC  9(008).
      *         Datum Produkteroeffnung i.d.F. JHJJMMTT
            05  'XX'-DTA-DATUM-SCHLIESSUNG  PIC  9(008).
      *         Datum Produktschliessung i.d.F. JHJJMMTT
            05  'XX'-DTA-DARLEHNSSUMME      PIC  9(012)V99.
      *         Ursprungskreditbetrag
            05  'XX'-DTA-DAT-KREDITZUSAGE   PIC  9(008).
      *         Datum der Kreditzusage i.F.JHJJMMTT
            05  'XX'-DTA-DAT-KREDITAUSZHLG  PIC  9(008).
      *         Datum der Darlehnsauszahlung i.d.F. JHJJMMTT = 0
            05  'XX'-DTA-KREDITART          PIC  X(006).
      *         immer Blank.
            05  'XX'-DTA-AKT-DARLEHENSBTRG
                                 PIC S9(11)V99 SIGN LEADING SEPARATE.
      *         Saldo (Aktueller Darlehnsstand)
            05  'XX'-DTA-SOLL-ZINSSATZ      PIC  9(002)V9999.
      *         Sollzinssatz in Prozent p.a.
            05  'XX'-DTA-RATE-EURO          PIC 9(012)V99.
      *         Rate in Euro.
            05  'XX'-DTA-DAT-FAELLIG        PIC 9(008).
      *         Datum letzte Rate i.d.F. JHJJMMTT.
            05  'XX'-DTA-DEL-KZ             PIC  X(001).
      *         DEL-Kennzeichen  ('D' oder Blank)
            05  'XX'-DTA-RESERVE            PIC  X(068).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 01                  *
      *                                                                *
      *     LAENGE:       200 BYTES               STAND: 09.03.2009    *
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
            05  'XX'-DTA-N-FILLER                PIC X(191).
      *         RESERVE
      *
      ******************************************************************
      *          E N D E    COPY-MEMBER DRKI2292                       *
      ******************************************************************

