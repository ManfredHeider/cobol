      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  17.08.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI2327.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI2327.CPv  $
      *
      *   Rev 2.3   Nov 12 2012 09:26:54   t13ex39
      *AKI01168: Anzeige der Buchungen zu DWS-Altersvorsorgeverträgen
      *
      *   Rev 2.2   Oct 27 2009 11:55:10   t13ex50
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.1   Sep 11 2009 17:20:40   t13ex52
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.0   Aug 17 2009 15:48:52   t13ex52
      *Archiverstellung
      *
      *--------------------------------------------------------------- *
      *    COPY-MEMBER ZUM ERMITTLUNGSPROGRAMM                         *
      *     - AUSGABEDATEI DWS Aufbau wie DTA-Eingabe, erweitert       *
      *       mit einem Feld fuer das Aenderungskennzeichen.           *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *18.08.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *28.08.09! Feld DWS-AUS-SCHLUSSTAG vom Format   ! HSL     !      *
      *        ! N08 auf Format X10 geaendert.        !         !      *
      *        !                                      !         !      *
      *07.11.12! DWS-AUS-BUCHUNGSSCHLUESSEL jetzt     !         !      *
      *        ! pic 9(4)                             !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *
      **************************************************************** *
      *                                                                *
      *                    DWS Depottransaktionen                      *
      *                 Ausgabe - D A T E N S A T Z                    *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 18.08.2009      *
      *                                                                *
      **************************************************************** *
      *
       01 DRKI2327.
      *
         05  DWS-AUS-SA                 PIC  X(002).
      *      Satzart = '19'
         05  DWS-AUS-GES                PIC  9(002).
      *      Gesellschafts-Nr. = 48
         05  DWS-AUS-VTRGNR             PIC  9(010).
      *      3-stellige DWS-Filialnummer
      *      7-stellige DWS-Kundennummer)
         05  DWS-AUS-VTRG-UNTERNUMMER   PIC  9(002).
      *      Wird als Ordnungskennzeichen bei
      *      DVAG-Verarbeitung intern benötigt;
      *      laufende Nummer beginnend mit 00.
      *      Details s. gleiches Feld im
      *      Datensatz Vertragsdaten DWS
         05  DWS-AUS-PORTFOLIONR        PIC  X(010).
      *      Inhalt = T123456701
         05  DWS-AUS-WKN                PIC  X(006).
      *      Wertpapierkennnummer
         05  DWS-AUS-ISIN               PIC  X(012).
      *      ISIN, sofern vorhanden.
         05  DWS-AUS-ART-DES-PAPIERS    PIC  9.
      *      nur Stücke möglich
      *      1 = Stücke
         05  DWS-AUS-STUECKANZAHL       PIC 9(012)V9999.
      *
         05  DWS-AUS-ORIGINAL-WHRG      PIC X(003).
      *      Original-Währung des Papiers.
      *      Angabe ISO-Währungscode.
      *      Bei Riestervertraegen immer EURO.
      *
         05  DWS-AUS-BRUTTOUMSATZ       PIC  9(010)V99.
      *
280809*  05  DWS-AUS-SCHLUSSTAG         PIC  9(008).
      *      Schlusstag i.d.F JHJJMMTT
280809   05  DWS-AUS-SCHLUSSTAG         PIC  X(010).
      *      Schlusstag i.d.F JHJJ-MM-TT
      *
071112*  05  DWS-AUS-BUCHUNGSSCHLUESSEL PIC  S9(03).
071112   05  DWS-AUS-BUCHUNGSSCHLUESSEL PIC  S9(04).
      *      Ausprägungen zum Buchungsschlüssel:
      *      siehe Pkt. 5 im Dokument
      *      DWS_Datenspiegel_DVAG_Stand 2009_07_27
      *
         05  DWS-AUS-STORNO-KNZ         PIC  X.
      *      Blank = kein Storno
      *      1 =     Storno
      *
071112*  05  DWS-AUS-RESERVE            PIC  X(109).
071112   05  DWS-AUS-RESERVE            PIC  X(108).
      *
         05  DWS-AUS-AKTUAL-KNZ         PIC  X.
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI2327                 *
      ******************************************************************
