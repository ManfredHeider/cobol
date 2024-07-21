      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2326.CPY  $
      *    $Revision:   2.9  $
      *
      *    $Log:   L:/mf/CPY/DRKI2326.CPv  $
      *
      *   Rev 2.9   Mar 28 2017 07:57:36   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.8   Aug 15 2012 09:51:50   t13ex39
      *AKI01168: Anzeige der Buchungen zu DWS-Altersvorsorgeverträgen
      *
      *   Rev 2.7   Oct 27 2009 12:14:18   t13ex50
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.6   Sep 11 2009 17:19:34   t13ex52
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.5   Aug 18 2009 14:16:50   t13ex52
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.4   Aug 18 2009 14:06:30   t13ex52
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.3   Aug 18 2009 13:21:40   t13ex52
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.2   Aug 18 2009 12:05:28   t13ex52
      *AKI00844: Copy fuer DTA DWS-Depottransaktionen
      *
      *   Rev 2.1   Aug 18 2009 10:57:38   t13ex52
      *AKI00844: DWS-Datenspiegel für DWS-Vorsorgeprodukte
      *
      *   Rev 2.0   Aug 17 2009 15:48:46   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - EINGABEDATEI DWS DTA AENDERUNGS- / KOMPLETTBESTAND       *
      *       KORREKTUREN MIT VOR- UND NACHSATZ.                       *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *17.08.09! NEU                                  ! HSL     !      *
      * ----------------!-----------------------------!---------!------*
      *AKI01168!01.08.12! DWS-TRANSAKTIONSSCHLÜSSEL   ! WHO     ! 2.0  *
      * ----------------!-----------------------------!---------!------*
      *FKI01544!28.03.17! FVD entfernt                ! HHU     !      *
      *        !        !                             !         !      *
      *        !        !                             !         !      *
      *        !        !                             !         !      *
      *        !        !                             !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 01                   *
      *                                                                *
      *     LAENGE:       200 BYTES               STAND: 17.08.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
       01 DRKI2326.
 
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 DWS-DTA-VORSATZ.
      *
            05  DWS-DTA-V-SA                          PIC X(002).
      *         SATZART = '00'
            05  DWS-DTA-V-ABSENDER                    PIC X(008).
      *         ABSENDER = 'DWS'
            05  DWS-DTA-V-BUCHUNGSDATUM               PIC 9(008).
      *         Buchungsdatum = 'JJJMMTT'
            05  DWS-DTA-V-EMPFAENGER                  PIC X(004).
      *         EMPFAENGER = 'DVAG' 'ALLF'
            05  DWS-DTA-V-TEXT                        PIC X(008).
      *         INHALTSANGABE:
      *         DEPOTTRANSAKTIONENE TÄGLICH   'BUCHT'
      *         DEPOTTRANSAKTIONENE KOMPLETT  'BUCHK'
      *
            05  DWS-DTA-V-WKZ                         PIC X(003).
      *         WÄHRUNGSKENNZEICHEN = 'EUR'
            05  DWS-DTA-V-FILLER                      PIC X(167).
      *         RESERVE
      *
      **************************************************************** *
      *                                                                *
      *                    DWS Depottransaktionen                      *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 17.08.2009      *
      *                                                                *
      **************************************************************** *
      *
          03 DWS-DTA-DEPOTTRANSAKTIONEN REDEFINES DWS-DTA-VORSATZ.
      *
            05  DWS-DTA-SA                 PIC  X(002).
      *         Satzart = '19'
            05  DWS-DTA-GES                PIC  9(002).
      *         Gesellschafts-Nr. = 48
            05  DWS-DTA-DPTTRANSAKTIONEN-KEY.
              10 DWS-DTA-VTRGNR.
                15 DWS-DTA-FILIAL-NR      PIC  9(003).
      *         3-stellige DWS-Filialnummer
                15 DWS-DTA-KNDNR          PIC  9(007).
      *         7-stellige DWS-Kundennummer)
              10 DWS-DTA-VTRG-ORDNUNGSNR    PIC  9(002).
      *         Wird als Ordnungskennzeichen bei
      *         DVAG-Verarbeitung intern benötigt;
      *         laufende Nummer beginnend mit 00.
      *         Details s. gleiches Feld im
      *         Datensatz Vertragsdaten DWS
            05  DWS-DTA-PORTFOLIONR        PIC  X(010).
      *         Inhalt = T123456701
            05  DWS-DTA-WKN                PIC  X(006).
      *         Wertpapierkennnummer
            05  DWS-DTA-ISIN               PIC  X(012).
      *         ISIN, sofern vorhanden.
            05  DWS-DTA-ART-DES-PAPIERS    PIC  9(001).
      *         nur Stücke möglich
      *         1 = Stücke
            05  DWS-DTA-STUECKANZAHL       PIC  9(012)V9999.
      *
            05  DWS-DTA-ORIGINAL-WHRG      PIC  X(003).
      *         Original-Währung des Papiers.
      *         Angabe ISO-Währungscode.
      *         Bei Riestervertraegen immer EURO.
            05  DWS-DTA-BRUTTOUMSATZ       PIC  9(010)V99.
      *         VORNAME DES KONTOINHABERS. Kann fuer Firmen und fuer
      *         Vertraege mit Datenschutzsperre leer sein.
            05  DWS-DTA-SCHLUSSTAG         PIC  9(008).
      *         Schlusstag i.d.F JHJJMMTT
      *
            05  DWS-DTA-BUCHUNGSSCHLUESSEL PIC  X(002).
      *         Ausprägungen zum Buchungsschlüssel:
      *         siehe Pkt. 5 im Dokument
      *         DWS_Datenspiegel_DVAG_Stand 2009_07_27
      *
            05  DWS-DTA-STORNO-KNZ         PIC  X(001).
      *         Blank = kein Storno
      *         1 =     Storno
      *
010812      05  DWS-TRANSAKTIONS-SCHL      PIC 9(003).
 
010812*     05  DWS-DTA-RESERVE            PIC  X(113).
010812      05  DWS-DTA-RESERVE            PIC  X(110).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 01                  *
      *                                                                *
      *     LAENGE:       200 BYTES               STAND: 17.08.2009    *
      *                                                                *
      *                                                                *
      ******************************************************************
      **** NACHSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 DWS-DTA-NACHSATZ REDEFINES DWS-DTA-VORSATZ.
      *
            05  DWS-DTA-N-SA                    PIC X(002).
      *         SATZART = '99'
            05  DWS-DTA-N-ANZAHL-RECORD         PIC 9(007).
      *         Hier wird die Anzahl der Datensätze dieser Datei
      *         eingestellt.
      *         Die Anzahl enthält nicht den Vorsatz und auch nicht
      *         diesen Nachsatz (nur die reinen Datensätze)
            05  DWS-DTA-N-FILLER                PIC X(191).
      *         RESERVE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI2326                 *
      ******************************************************************

