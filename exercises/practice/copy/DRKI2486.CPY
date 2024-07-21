      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  07.06.2011
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2011 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2486.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2486.CPv  $
      *
      *   Rev 2.2   Mar 27 2017 09:29:10   t13ae31
      *FKI01544: FVD Teil 1 alten Code entfernen COPY
      *
      *   Rev 2.1   Jul 11 2011 16:16:54   t13ex18
      *AKI01040: Erw. Deuba- inkl. DWS-Daten / Ausgabe in KI-Vertrag
      *
      *   Rev 2.0   Jun 07 2011 15:02:58   t13ex18
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - EINGABEDATEI DWS DTA AENDERUNGS- / KOMPLETTBESTAND       *
      *       'DWS-ZULAGEN' (MIT VOR- UND NACHSATZ)                    *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *06.06.11! NEU                                  ! MDI     !      *
      *--------------------------------------------------------------- *
      *24.03.17! FVD entfernt                         ! HHU     !      *
      *        !                                      !         !      *
      *                                                                *
      ******************************************************************
      *                                                                *
      *                   V O R S A T Z - SATZART 00                   *
      *                                                                *
      *     LAENGE:       100 BYTES               STAND: 06.06.2011    *
      *                                                                *
      *                                                                *
      ******************************************************************
       01 DRKI2486.
 
      **** VORSATZ(GUELTIG FUER ALLE SATZARTEN)
          03 DWS-DTA-VORSATZ.
      *
            05  DWS-DTA-V-SA                          PIC X(002).
      *         SATZART = '00'
            05  DWS-DTA-V-ABSENDER                    PIC X(008).
      *         ABSENDER = 'DWS'
            05  DWS-DTA-V-BUCHUNGSDATUM               PIC 9(008).
      *         Buchungsdatum = 'JJJJMMTT'
            05  DWS-DTA-V-EMPFAENGER                  PIC X(004).
      *         EMPFAENGER = 'DVAG', 'ALLF'
            05  DWS-DTA-V-TEXT                        PIC X(008).
      *         INHALTSANGABE:
      *         ZULAGEN             TÄGLICH   'ZULAGET '
      *         ZULAGEN             KOMPLETT  'ZULAGEK '
      *
            05  DWS-DTA-V-WKZ                         PIC X(003).
      *         WÄHRUNGSKENNZEICHEN = 'EUR'
            05  DWS-DTA-V-FILLER                      PIC X(067).
      *         RESERVE
      *
      **************************************************************** *
      *                                                                *
      *                         DWS-Zulagen                            *
      *                 D T A - D A T E N S A T Z                      *
      *                                                                *
      *     LAENGE:       100 BYTES               STAND: 06.06.2011    *
      *                                                                *
      **************************************************************** *
      *
          03 DWS-DTA-ZULAGEN            REDEFINES DWS-DTA-VORSATZ.
      *
            05  DWS-DTA-SA                 PIC  X(002).
      *         Satzart = '20'
            05  DWS-DTA-GES                PIC  9(002).
      *         Gesellschafts-Nr. = 48
            05  DWS-DTA-ZULAGEN-KEY.
              10 DWS-DTA-VTRGNR.
                15 DWS-DTA-FILIAL-NR       PIC  9(003).
      *         3-stellige DWS-Filialnummer
                15 DWS-DTA-KNDNR           PIC  9(007).
      *         7-stellige DWS-Kundennummer
              10 DWS-DTA-VTRG-ORDNUNGSNR   PIC  9(002).
      *         Wird als Ordnungskennzeichen bei
      *         DVAG-Verarbeitung intern benötigt;
      *         laufende Nummer beginnend mit 00.
      *         Details s. gleiches Feld im
      *         Datensatz Vertragsdaten DWS
            05  DWS-DTA-PORTFOLIONR        PIC  X(010).
      *         Portfolio-Nummer
            05  DWS-DTA-BEITRJAHR          PIC  9(004).
      *         Beitragsjahr (JHJJ)
            05  DWS-DTA-ZULAGEN-BEITRJAHR  PIC  9(011)V9(002).
      *         Zulagen für Beitragsjahr
            05  DWS-DTA-DELETE-KNZ         PIC  X(001).
      *         D =     Satz löschen
      *         S =     Datenschutzsperre aktiviert
      *         Befüllung gekoppelt an der Befüllung im Feld 21
      *         "Delete-Kennzeichen" im Datensatz Nr. 16, Vertragsdaten
      *         DWS, desselben Vertrags.
      *
            05  DWS-DTA-RESERVE            PIC  X(056).
      *
      ******************************************************************
      *                                                                *
      *                 N A C H S A T Z -  SATZART 99                  *
      *                                                                *
      *     LAENGE:       100 BYTES               STAND: 06.06.2011    *
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
            05  DWS-DTA-N-FILLER                PIC X(091).
      *         RESERVE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI2486                 *
      ******************************************************************

