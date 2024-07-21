      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drki0187.cpy  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:\MF\CPY\drki0187.cpv  $
      *
      *   Rev 2.2   07 Mar 2001 09:42:46   T13EX39
      *AKI00156 NEUER FOND
      *
      *   Rev 2.1   Dec 10 1999 17:42:06   T13EX50
      *AKI67
      *
      *   Rev 2.0   Feb 27 1999 12:53:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:54   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *  DRKI0187                                                    *
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER FÜR AUSGABEDATEI DER ERMITTLING DGI             *
      *  --------------------------------------------------------    *
      *                                                              *
      *  DATEINAME  =  P130006.KI066300                              *
      *                                                              *
      *  FESTE SATZLAENGE VON 250 BYTES                              *
      *                                                              *
      ****************************************************************
      *                                                              *
      *                                                              *
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AKI00156!24.10.00! zus. Fond                 ! WHO           *  *
      *         !        !                           !               *  *
      *         !        !                           !               *  *
      *         !        !                           !               *  *
      *         !        !                           !               *  *
      *         !        !                           !               *  *
      ****************************************************************
       01  DRKI0187.
        02 K-KSINFO.
           05  A-KSINFO-SATZ                 PIC X(333).
           05  A-KSINFO-SATZ-0 REDEFINES A-KSINFO-SATZ.
               10 A-VERARBEITUNGS-KZ         PIC X(001).
               10 A-SATZART                  PIC 9(003).
               10 A-KONS-NR                  PIC 9(007).
               10 A-DEPOT-NR.
241000            20 A-KUNDEN-NR.
241000               25 A-FONDS-NR           PIC 9(003).
241000               25 A-KUNDEN-NR-1        PIC X(008).
                  20 A-DEPOTUNTER-NR         PIC 9(002).
               10 A-DEPOT-INFO-KZ            PIC X(001).
               10 A-DEPOT-INFO-KZ-KZ         PIC X(001).
               10 A-SATZ                     PIC X(307).

               10 A-KSINFO-SATZ-20 REDEFINES A-SATZ.
                  20 A-ANREDE                PIC 9(002).
                  20 A-ANREDE-KZ             PIC X(001).
                  20 A-VORNAME               PIC X(030).
                  20 A-VORNAME-ALT           PIC X(030).
                  20 A-VORNAME-KZ            PIC X(001).
                  20 A-NACHNAME              PIC X(030).
                  20 A-NACHNAME-ALT          PIC X(030).
                  20 A-NACHNAME-KZ           PIC X(001).
                  20 A-STRASSE               PIC X(030).
                  20 A-STRASSE-KZ            PIC X(001).
                  20 A-PLZ                   PIC X(007).
                  20 A-PLZ-KZ                PIC X(001).
                  20 A-ORT                   PIC X(030).
                  20 A-ORT-KZ                PIC X(001).
                  20 A-GEB-DATUM             PIC 9(008).
                  20 A-GEB-DATUM-KZ          PIC X(001).
                  20 A-LAND                  PIC 9(003).
                  20 A-LAND-KZ               PIC X(001).
                  20 A-BERUF                 PIC 9(003).
                  20 A-BERUF-KZ              PIC X(001).
                  20 A-TELEFON               PIC X(015).
                  20 A-TELEFON-KZ            PIC X(001).
                  20 A-FAX                   PIC X(015).
                  20 A-FAX-KZ                PIC X(001).
                  20 A-BLZ                   PIC 9(008).
                  20 A-BLZ-KZ                PIC X(001).
                  20 A-KONTO-NR              PIC 9(012).
                  20 A-KONTO-NR-KZ           PIC X(001).
                  20 A-EMPFAENGER            PIC X(040).
                  20 A-EMPFAENGER-KZ         PIC X(001).

               10 A-KSINFO-SATZ-30 REDEFINES A-SATZ.
                  20 A-VINFO.
                     25 A-INTVSNR            PIC 9(007).
                     25 A-INTVSNR-KZ         PIC X(001).
                     25 A-I-PRZ              PIC 9(002).
                     25 A-I-PRZ-KZ           PIC X(001).
                     25 A-VBNR               PIC 9(007).
                     25 A-VBNR-KZ            PIC X(001).
                  20 A-BEGINN                PIC 9(008).
                  20 A-BEGINN-KZ             PIC X(001).
                  20 A-LAST-BETRAG           PIC S9(7)V9(2).
                  20 A-LAST-BETRAG-KZ        PIC X(001).
                  20 A-LAST-ZAHLRHYTHMUS     PIC 9(002).
                  20 A-LAST-ZAHLRHYTHMUS-KZ  PIC X(001).
                  20 A-DEPOT-WAEHRUNG        PIC X(003).
                  20 A-DEPOT-WAEHRUNG-KZ     PIC X(001).
                  20 FILLER                  PIC X(262).

               10 A-KSINFO-SATZ-40 REDEFINES A-SATZ.
                  20 A-POS-WAEHRUNG          PIC X(003).
                  20 A-POS-WAEHRUNG-KZ       PIC X(001).
                  20 A-VERB-DATUM            PIC 9(008).
                  20 A-VERB-DATUM-KZ         PIC X(001).
                  20 A-BETRAG-POS            PIC S9(11)V9(2) COMP-3.
                  20 A-BETRAG-POS-KZ         PIC X(001).
                  20 A-BESTAND               PIC 9(007)V9(4) COMP-3.
                  20 A-BESTAND-KZ            PIC X(001).
                  20 A-BUCH-BESTAND          PIC 9(007)V9(4) COMP-3.
                  20 A-SOLL-HABEN-KZ         PIC X(001).
                  20 A-STORNO-KZ             PIC X(001).
                  20 FILLER                  PIC X(271).

