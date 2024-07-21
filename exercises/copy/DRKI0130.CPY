      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0130.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRKI0130.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 12:53:06   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   27 May 1998 16:35:28   P13SP13
      *Neuerstellung
      *
      *   Rev 1.0   Jan 15 1996 17:15:20   T13AE22
      *Initial revision.
      *
      ****************************************************************
      *  DRKD0020                                                    *
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER FÜR SORT. ORIGINAL-KUNDENKOMPLETTBESTAND DGI    *
      *  --------------------------------------------------------    *
      *                                                              *
      *  DATEINAME  =  P130006.DGIKD.SORT  (QSAM-DATEI)              *
      *                                                              *
      *  FESTE SATZLAENGE VON 250 BYTES                              *
      *                                                              *
      ****************************************************************
      * ERSTELLT AM : 27.04.1998                                     *
      *         VON : C. von der Goltz                               *
      *                                                              *
      *                                                              *
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AKI00001!27.04.98! NEUERSTELLUNG             ! von der Goltz *
      *         !        !                           !               *
      *         !        !                           !               *
      ****************************************************************


       01  DRKI0130.
        02 KSINFO.
           05  KSINFO-SATZ                              PIC X(250).
           05  KSINFO-SATZ-0 REDEFINES KSINFO-SATZ.
               10 SATZART                               PIC 9(003).
               10 KONS-NR                               PIC 9(007).
               10 DEPOT-NR.
                  20 FONDS-NR                           PIC 9(003).
                  20 KUNDEN-NR                          PIC X(008).
                  20 DEPOTUNTER-NR                      PIC 9(002).
               10 DEPOT-INFO-KZ                         PIC X(001).
               10 SATZ                                  PIC X(226).

      *           * VORSATZ
      *           * ALLE DATUEMER IM FORMAT = 'JJJJMMTT'
               10 KSINFO-SATZ-1 REDEFINES SATZ.
                  20 ERST-PROGRAMM                      PIC X(008).
                  20 ERST-DATUM                         PIC 9(008).
                  20 SEL-DATUM-VON                      PIC 9(008).
      *           *  SELEKTIONSZEITRAUM FÜR POSITIONEN
                  20 SEL-DATUM-BIS                      PIC 9(008).
                  20 ABSENDER                           PIC X(030).
                  20 INFO-AUSWAHL                       PIC X(001).
      *           *  INFO-AUSWAHL 4 = KOMPLETTBESTAND
                  20 FILLER                             PIC X(163).

      *           * KUNDENINFORMATION (PRO INHABER, MAX. 10)
               10 KSINFO-SATZ-20 REDEFINES SATZ.
                  20 INHABER-NR                         PIC 9(002).
                  20 ANREDE                             PIC 9(002).
                  20 ADELS-TITEL                        PIC 9(002).
                  20 AKAD-TITEL                         PIC 9(002).
                  20 VORNAME                            PIC X(030).
                  20 NACHNAME                           PIC X(030).
                  20 STRASSE                            PIC X(030).
                  20 PLZ                                PIC X(007).
                  20 ORT                                PIC X(030).
                  20 ZUSATZTEXT                         PIC X(030).
                  20 GEB-DATUM                          PIC 9(008).
                  20 VERSAND-KZ                         PIC 9(002).
                  20 LAND                               PIC 9(003).
                  20 BERUF                              PIC 9(003).
                  20 TELEFON                            PIC X(015).
                  20 FAX                                PIC X(015).
                  20 KUNDEN-WAEHRUNG                    PIC X(003).
                  20 FILLER                             PIC X(012).

      *           * VERTRAGS- / DEPOTINFORMATION (PRO DEPOT 1 MAL)
               10 KSINFO-SATZ-30 REDEFINES SATZ.
                  20 VINFO.
                     25 FILLER                          PIC X(009).
                     25 INTVSNR                         PIC 9(007).
                     25 I-PRZ                           PIC 9(002).
                     25 VBNR                            PIC 9(007).
                     25 VB-PRZ                          PIC 9(001).
                     25 FILLER                          PIC X(074).
                  20 TARIF                              PIC 9(002).
                  20 VERTRAGSART                        PIC 9(002).
                  20 DYN-PROZ                           PIC 9(002)V99.
                  20 SPERR-KZ                           PIC 9(002).
                  20 RUECK-PR            PIC 9(007)V9(006)  COMP-3.
                  20 BESTAND             PIC 9(007)V9(004)  COMP-3.
                  20 BESTAND-WERT        PIC 9(009)V9(002)  COMP-3.
                  20 BEGINN              PIC 9(008).
                  20 SPARZIEL            PIC 9(009)V9(002)  COMP-3.
                  20 RATE                PIC 9(007)V9(002)  COMP-3.
                  20 ERSTE-ZAHLUNG                   PIC 9(008).
                  20 WKN                             PIC X(012).
                  20 LAST-ANZ                        PIC 9(002).
                  20 LAST-BETRAG         PIC 9(007)V9(002)  COMP-3.
                  20 LAST-ZAHLRHYTHUMS               PIC 9(002).
                  20 ENTN-ANZ                        PIC 9(002).
                  20 ENTN-BETRAG         PIC 9(007)V9(002)  COMP-3.
                  20 ENTN-ZAHLRHYTHUMS               PIC 9(002).
                  20 DEPOT-WAEHRUNG                  PIC X(003).
                  20 FILLER                          PIC X(037).

      *           * POSITIONSINFORMATION (PRO UMSATZ 1 MAL)
               10 KSINFO-SATZ-40 REDEFINES SATZ.
                  20 BELEG-NR            PIC 9(009)         COMP-3.
                  20 BELEG-UNTERNR       PIC 9(002)         COMP-3.
                  20 BELEG-POS           PIC 9(005)         COMP-3.
                  20 UMSATZ-SCHL         PIC 9(002).
                  20 POS-WAEHRUNG        PIC X(003).
                  20 PREISTAG            PIC 9(008).
                  20 PREIS               PIC 9(007)V9(006)  COMP-3.
                  20 BETRAG-POS          PIC S9(011)V9(002)  COMP-3.
                  20 ANTEILE-POS         PIC S9(009)V9(004)  COMP-3.
                  20 BONI-AN-KONS        PIC S9(011)V9(002)  COMP-3.
                  20 TARIF-POS           PIC 9(002).
                  20 STORNO-KZ           PIC X(001).
                  20 RABATT              PIC 9(002)V9(002)  COMP-3.
                  20 ERSTER-UMSATZ-KZ    PIC X(001).
                  20 VERB-DATUM          PIC 9(008).
                  20 FILLER              PIC X(160).

      *           * TAGESPREISE DER FONDS (PRO FONDS 1 MAL)
               10 KSINFO-SATZ-600 REDEFINES SATZ.
                  20 PREISTAG-600        PIC 9(008).
                  20 AUSGABEPREIS        PIC S9(007)V9(006)  COMP-3.
                  20 RUECKNAHMEPREIS     PIC S9(007)V9(006)  COMP-3.
                  20 ZWIST-EKST          PIC S9(007)V9(006)  COMP-3.
                  20 ZWIST-ZAST          PIC S9(007)V9(006)  COMP-3.
                  20 FILLER              PIC X(190).

               10 KSINFO-SATZ-800 REDEFINES SATZ.
                  20 TABELLEN-NR            PIC 9(003).
                  20 TABELLEN-SCHL          PIC 9(003).
                  20 SCHLUESSEL-BEZ         PIC X(030).
                  20 FILLER                 PIC X(190).

               10 KSINFO-SATZ-999 REDEFINES SATZ.
                  20 ANZ-SAETZE-KUNDE       PIC 9(009)      COMP-3.
                  20 ANZ-SAETZE-VERTRAG     PIC 9(009)      COMP-3.
                  20 ANZ-SAETZE-POSITION    PIC 9(009)      COMP-3.
                  20 ANZ-SAETZE-WAEHRUNG    PIC 9(009)      COMP-3.
                  20 ANZ-SAETZE-PREISE      PIC 9(009)      COMP-3.
                  20 ANZ-SAETZE-TABELLEN    PIC 9(009)      COMP-3.
                  20 FILLER                 PIC X(196).
      *
