      *----------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  26.08.2009
      *            Kategorie  :  Copystrecke
      *----------------------------------------------------------------*
      *
      *----------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *----------------------------------------------------------------*
      *    $Workfile:   DRKI2357.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRKI2357.CPv  $
      *
      *   Rev 2.2   Oct 27 2009 11:55:24   t13ex50
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.1   Sep 11 2009 17:20:56   t13ex52
      *AKI00844: DWS-DepotTransaktionen
      *
      *   Rev 2.0   Aug 26 2009 18:10:44   t13ex52
      *Archiverstellung
      *
      *    DATEITYP  = QSAM                                            *
      *    Länge = 200 Bytes                                           *
      *----------------------------------------------------------------*
      *    COPY-MEMBER ZUM Aktualisierungsprogramm                     *
      *     - AUSGABEDATEI DWS Bank Guthabenbuchung  das Programm      *
      *       PMKI1019.                                                *
      *--------------------------------------------------------------- *
      *    LOGBUCH                                                     *
      *DATUM   !      ÄNDERUNG                        !NAME     !VERS. *
      *AUFTRAG !                                      !         !      *
      *--------!--------------------------------------!---------!----- *
      *26.08.09! NEU                                  ! HSL     !      *
      *        !                                      !         !      *
      *28.08.09! Feld DWS-BNK-BUCH-SCHLUSSTAG von For-! HSL     !      *
      *        ! mat N08 auf Format X10 geaendert.    !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *        !                                      !         !      *
      *--------------------------------------------------------------- *
      *                                                                *
      *
      **************************************************************** *
      *                                                                *
      *           DWS Depottransaktionen Bank-Buchanteile (BNK)        *
      *                 Ausgabe - D A T E N S A T Z                    *
      *                                                                *
      *                     (BetaVersion 1.0)                          *
      *                                                                *
      *     LAENGE:   200 BYTES                 STAND: 25.08.2009      *
      *                                                                *
      **************************************************************** *
      *
       01 DRKI2357.
         05  DWS-BNK-SA                      PIC  X(002).
      *      Satzart = '19'
         05  DWS-BNK-GES                     PIC  9(002).
      *      Gesellschafts-Nr. = 48
         05  DWS-BNK-VTRGNR                  PIC  9(010).
      *      3-stellige DWS-Filialnummer
      *      7-stellige DWS-Kundennummer)
         05  DWS-BNK-VTRG-UNTERNUMMER        PIC  9(002).
      *      Wird als Ordnungskennzeichen bei
      *      DVAG-Verarbeitung intern benötigt;
      *      laufende Nummer beginnend mit 00.
      *      Details s. gleiches Feld im
      *      Datensatz Vertragsdaten DWS
         05  DWS-BNK-PORTFOLIONR             PIC  X(010).
      *      Inhalt = T123456701
         05  DWS-BNK-WP-KENN-NR              PIC  X(006).
      *      Wertpapierkennnummer
         05  DWS-BNK-ISIN                    PIC  X(012).
      *      ISIN, sofern vorhanden.
         05  DWS-BNK-BUCH-ART-DES-PAPIERS    PIC  9.
      *      nur Stücke möglich
      *      1 = Stücke
         05  DWS-BNK-BUCH-STUECKANZAHL       PIC 9(012)V9999.
      *
         05  DWS-BNK-BUCH-ORIGINAL-WHRG      PIC X(003).
      *      Original-Währung des Papiers.
      *      Angabe ISO-Währungscode.
      *      Bei Riestervertraegen immer EURO.
      *
         05  DWS-BNK-BUCH-BRUTTOUMSATZ       PIC  9(010)V99.
      *
280809*  05  DWS-BNK-BUCH-SCHLUSSTAG         PIC  9(008).
      *      Schlusstag i.d.F JHJJMMTT
      *
280809   05  DWS-BNK-BUCH-SCHLUSSTAG         PIC  X(010).
      *      Schlusstag i.d.F JHJJ-MM-TT

         05  DWS-BNK-BUCH-BUCHUNGSKEY        PIC  S9(03).
      *      Ausprägungen zum Buchungsschlüssel:
      *      siehe Pkt. 5 im Dokument
      *      DWS_Datenspiegel_DVAG_Stand 2009_07_27
      *
         05  DWS-BNK-BUCH-WAEHRUNGS-KNZ      PIC  X.
      *
280809*  05  DWS-BNK-RESERVE                 PIC  X(098).
280809   05  DWS-BNK-RESERVE                 PIC  X(109).
      *
         05  DWS-BNK-AKTUAL-KNZ              PIC  X.
      *        I = INSERT
      *        U = UPDATE
      *        D = DELETE
      *
      ******************************************************************
      *                E N D E    COPY-MEMBER DRKI2357                 *
      ******************************************************************
