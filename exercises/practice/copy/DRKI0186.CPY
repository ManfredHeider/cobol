      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRKI0186.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRKI0186.CPv  $
      *
      *   Rev 2.3   Jul 06 2004 12:01:56   t13ex27
      *FKI00862: DGI VERVERTRZUSTANDID 10 -> 11
      *
      *   Rev 2.2   07 Mar 2001 09:42:44   T13EX39
      *AKI00156 NEUER FOND
      *
      *   Rev 2.1   Dec 10 1999 17:41:44   T13EX50
      *AKI67
      *
      *   Rev 2.0   Feb 27 1999 12:53:32   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   29 Sep 1997 11:55:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *  DRKI0186                                                    *
      ****************************************************************
      *                                                              *
      *  COPY-MEMBER FÜR KOMPLETTBESTAND DGI                         *
      *  --------------------------------------------------------    *
      *                                                              *
      *  DATEINAME  =  T130006.KI066100.I1.CL                        *
      *                                                              *
      *  FESTE SATZLAENGE VON 250 BYTES                              *
      *                                                              *
      ****************************************************************
      *                                                              *
      *                                                              *
      * LOGBUCH                                                      *
      * AUFTRAG ! DATUM  ! AENDERUNG                 ! NAME          *
      * --------+--------+---------------------------+---------------*
      * AKI00156!24.10.00! zus. Fond                 ! WHO           *  
      * FKI00862!01.07.04! Ablauf-KZ für aufgelöste  ! I. Frenzel    *  
      *         !        ! Depots                    !               *  
      *         !        !                           !               *  
      *         !        !                           !               *  
      *         !        !                           !               *  
      ****************************************************************
       01  DRKI0186.
        02 K-KSINFO.
           05  K-KSINFO-SATZ                 PIC X(252).
           05  K-KSINFO-SATZ-0 REDEFINES K-KSINFO-SATZ.
               10 K-KEY.
241000            15 K-KUNDEN-NR.
241000               25 K-FONDS-NR           PIC 9(003).
241000               25 K-KUNDEN-NR-1        PIC X(008).
                  15 K-SATZART               PIC 9(003).
                  15 K-DEPOTUNTER-NR         PIC 9(002).
                  15 K-KONS-NR               PIC 9(007).
                  15 K-INHABER-NR-KEY        PIC 9(002).
               10 K-DEPOT-INFO-KZ            PIC X(001).
               10 K-SATZ                     PIC X(226).

      *           * KUNDENINFORMATION (PRO INHABER, MAX. 10)
               10 K-KSINFO-SATZ-20 REDEFINES K-SATZ.
                  20 K-INHABER-NR            PIC 9(002).
                  20 K-ANREDE                PIC 9(002).
                  20 K-ADELS-TITEL           PIC 9(002).
                  20 K-AKAD-TITEL            PIC 9(002).
                  20 K-VORNAME               PIC X(030).
                  20 K-NACHNAME              PIC X(030).
                  20 K-STRASSE               PIC X(030).
                  20 K-PLZ                   PIC X(007).
                  20 K-ORT                   PIC X(030).
                  20 K-ZUSATZTEXT            PIC X(030).
                  20 K-GEB-DATUM             PIC 9(008).
                  20 K-VERSAND-KZ            PIC 9(002).
                  20 K-LAND                  PIC 9(003).
                  20 K-BERUF                 PIC 9(003).
                  20 K-TELEFON               PIC X(015).
                  20 K-FAX                   PIC X(015).
                  20 K-KUNDEN-WAEHRUNG       PIC X(003).
                  20 FILLER                  PIC X(012).

      *           * VERTRAGS- / DEPOTINFORMATION (PRO DEPOT 1 MAL)
               10 K-KSINFO-SATZ-30 REDEFINES K-SATZ.
                  20 K-VINFO.
                     25 FILLER               PIC X(009).
                     25 K-INTVSNR            PIC 9(007).
                     25 K-I-PRZ              PIC 9(002).
                     25 K-VBNR               PIC 9(007).
                     25 K-VB-PRZ             PIC 9(001).
                     25 FILLER               PIC X(074).
                  20 K-TARIF                 PIC 9(002).
                  20 K-VERTRAGSART           PIC 9(002).
                  20 K-DYN-PROZ              PIC 9(2)V9(2).
                  20 K-SPERR-KZ              PIC 9(002).
                  20 K-RUECK-PR              PIC 9(7)V9(6)  COMP-3.
                  20 K-BESTAND               PIC 9(7)V9(4)  COMP-3.
                  20 K-BESTAND-WERT          PIC 9(9)V9(2)  COMP-3.
                  20 K-BEGINN                PIC 9(008).
                  20 K-SPARZIEL              PIC 9(9)V9(2)  COMP-3.
                  20 K-RATE                  PIC 9(7)V9(2)  COMP-3.
                  20 K-ERSTE-ZAHLUNG         PIC 9(008).
                  20 K-WKN                   PIC X(012).
                  20 K-LAST-ANZ              PIC 9(002).
                  20 K-LAST-BETRAG-30        PIC S9(7)V9(2) COMP-3.
                  20 K-LAST-ZAHLRHYTHUMS-30  PIC 9(002).
                  20 K-ENTN-ANZ              PIC 9(002).
                  20 K-ENTN-BETRAG           PIC 9(7)V9(2)  COMP-3.
                  20 K-ENTN-ZAHLRHYTHUMS     PIC 9(002).
                  20 K-DEPOT-WAEHRUNG        PIC X(003).
010704*           20 FILLER                  PIC X(037).
010704            20 K-ABLAUF-KZ             PIC X(001).
010704            20 FILLER                  PIC X(036).

      *           * BANKVERBINDUNG
               10 K-KSINFO-SATZ-200 REDEFINES K-SATZ.
                  20 K-BLZ                   PIC 9(008).
                  20 K-KONTO-NR              PIC 9(012).
                  20 K-EMPFAENGER            PIC X(040).
                  20 K-LAST-BETRAG           PIC S9(7)V9(2).
                  20 K-LAST-ZAHLRHYTHUMS     PIC 9(002).
                  20 FILLER                  PIC X(155).

