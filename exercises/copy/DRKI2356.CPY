      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  25.08.2009
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2356.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2356.CPv  $
      *
      *   Rev 2.3   Nov 12 2012 09:26:44   t13ex39
      *AKI01168: Anzeige der Buchungen zu DWS-Altersvorsorgeverträgen
      *
      *   Rev 2.2   Oct 27 2009 11:55:18   t13ex50
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.1   Sep 11 2009 17:20:46   t13ex52
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.0   Aug 25 2009 17:45:12   t13ex52
      *Archiverstellung
      *
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - AUSGABEDATEI DWS Aufbau wie Eingabe-Datei fuer Programm  *
      *       PMKI1018 erweitert um ein Feld Auftrags-ID.              *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.08.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *28.08.09! Feld DWS-DT-BUCH-SCHLUSSTAG von For- ! HSL     !      *
      *        ! mat N08 auf Format X10 geaendert.    !         !      *
      *--------!--------------------------------------!---------!----- *
      *AKI01168!                                      !         !      *
      *07.11.12! DWS-DT-BUCH-BUCHUNGSKEY jetzt        !         !      *
      *        ! pic 9(4)                             !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *
      **************************************************************** *
      *                                                                *
      *                    DWS Depottransaktionen (DT)                 *
      *                 Ausgabe - D A T E N S A T Z                    *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   213 BYTES                 STAND: 25.08.2009      *
      *                                                                *
      **************************************************************** *
      *
       01 DRKI2356.
      *
         05  DWS-DT-VERTRG-ID                 PIC  X(013).
         05  DWS-DT-GESAMT.
           15  DWS-DT-SA                      PIC  X(002).
      *        Satzart = '19'
           15  DWS-DT-GES                     PIC  9(002).
      *        Gesellschafts-Nr. = 48
           15  DWS-DT-VTRGNR                  PIC  9(010).
      *        3-stellige DWS-Filialnummer
      *        7-stellige DWS-Kundennummer)
           15  DWS-DT-VTRG-UNTERNUMMER        PIC  9(002).
      *        Wird als Ordnungskennzeichen bei
      *        DVAG-Verarbeitung intern benötigt;
      *        laufende Nummer beginnend mit 00.
      *        Details s. gleiches Feld im
      *        Datensatz Vertragsdaten DWS
           15  DWS-DT-PORTFOLIONR             PIC  X(010).
      *        Inhalt = T123456701
           15  DWS-DT-WP-KENN-NR              PIC  X(006).
      *        Wertpapierkennnummer
           15  DWS-DT-ISIN                    PIC  X(012).
      *        ISIN, sofern vorhanden.
           15  DWS-DT-BUCH-ART-DES-PAPIERS    PIC  9.
      *        nur Stücke möglich
      *        1 = Stücke
           15  DWS-DT-BUCH-STUECKANZAHL       PIC 9(012)V9999.
      *
           15  DWS-DT-BUCH-ORIGINAL-WHRG      PIC X(003).
      *        Original-Währung des Papiers.
      *        Angabe ISO-Währungscode.
      *        Bei Riestervertraegen immer EURO.
      *
           15  DWS-DT-BUCH-BRUTTOUMSATZ       PIC  9(010)V99.
      *
280809*    15  DWS-DT-BUCH-SCHLUSSTAG         PIC  9(008).
      *        Schlusstag i.d.F JHJJMMTT
      *
           15  DWS-DT-BUCH-SCHLUSSTAG         PIC  X(010).
280809*        Schlusstag i.d.F JHJJ-MM-TT

071112*    15  DWS-DT-BUCH-BUCHUNGSKEY        PIC  S9(03).
ß71112     15  DWS-DT-BUCH-BUCHUNGSKEY        PIC  S9(04).
      *        Ausprägungen zum Buchungsschlüssel:
      *        siehe Pkt. 5 im Dokument
      *        DWS_Datenspiegel_DVAG_Stand 2009_07_27
      *
           15  DWS-DT-STORNO-KNZ              PIC  X.
      *      Blank = kein Storno
      *      1 =     Storno
      *
071112*    15  DWS-DT-RESERVE                 PIC  X(109).
071112     15  DWS-DT-RESERVE                 PIC  X(108).

           15  DWS-DT-AKTUAL-KNZ              PIC  X.
      *          I = INSERT
      *          U = UPDATE
      *          D = DELETE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI2356                 *
      ******************************************************************
