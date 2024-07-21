      *****************************************************************
      *    COPY-MEMBER: DRPA1956
      *
      *   <KURZE FACHLICHE BESCHREIBUNG>
      *    EX POST KOSTENAUSWEIS SCHNITTSTELLE ZWISCHEN DAISY UND
      *    DEUTSCHE BANK (GES 48)
      *
      *
      *****************************************************************
      *****************************************************************
      * LOGBUCH:                                                      *
      * AUFTRAG    ! DATUM  ! AENDERUNG                    ! NAME     *
      * --------------------------------------------------------------*
      * DSY03594   !02.07.21! NEU (max. Länge VB 27421)    ! U130076  *
      * DSY03594   !07.09.21! ERWEITERUNG (neu VB 27415)   ! U130076  *
      *            !        !                              !          *
      *            !        !                              !          *
      *                                                               *
      * ENDE-LOGBUCH                                                  *
      *****************************************************************
           10  KOSTENAUSWEIS.
070921         15 KOSTENAUSWEIS-ANZ-FINST     PIC  999.              
070921         15 FILLER                      PIC  X(001) VALUE ';'. 
               15 KOSTENAUSWEIS-DEPOT.
      *           Filialnummer (ersten 3 Stellen der Depotnr)
                  20 DB-FILIAL-NR             PIC  X(003).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Kontonummer (Stelle 4 - 10 der Depotnr)
                  20 DB-KONTO-NR              PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Unterkonto (Stelle 11 - 12 der Depotnr)
                  20 DB-UNTERKONTO            PIC  X(002).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Währung
                  20 DB-WAEHRUNG              PIC  X(003).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Bankleistungstyp (immer space)
                  20 DB-BANKLTYP              PIC  X(001) VALUE SPACE.
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Pilotkunde (immer space)
                  20 DB-PILOTKUNDE            PIC  X(001) VALUE SPACE.
                  20 FILLER                   PIC  X(001) VALUE ';'.
                  20 VB-DATEN.
      *              VB ADRZEILE 1 (V1), Name
                     25 ADRZEILE1             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 2 (V2)
                     25 ADRZEILE2             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 3 (V3)
                     25 ADRZEILE3             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 4 (V4)
                     25 ADRZEILE4             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 5 (V5)
                     25 ADRZEILE5             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 6 (V6)
                     25 ADRZEILE6             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 7 (V7)
                     25 ADRZEILE7             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 8 (V8)
                     25 ADRZEILE8             PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 9 (V9)
                     25 ADRZEILE9             PIC  X(050).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 10 (V10)
                     25 ADRZEILE10            PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *              VB ADRZEILE 11 (V11)
                     25 ADRZEILE11            PIC  X(060).
                     25 FILLER                PIC  X(001) VALUE ';'.
      *           Kalenderjahr (B)
                  20 BD-KALENDER-JAHR         PIC  X(004).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Berichtszeitraum TT.MM. - TT.MM.JJJJ
                  20 BD-ZEITRAUM              PIC  X(019).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Depotgebühren in Euro (M)
                  20 DP-PREIS-EUR             PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Depotgebühren in % (M1)
                  20 DP-PREIS-PROZ            PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Dienstleistungskosten in EUR (N1)
                  20 DL-SUMMEN-EUR            PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Dienstleistungskosten in % (N2)
                  20 DL-SUMMEN-PROZ           PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Provision aus Ausgabeaufschlag in EUR (O1)
                  20 DL-SUMMEN-PROV-EUR       PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Provision aus Ausgabeaufschlag in % (O2)
                  20 DL-SUMMEN-PROV-PROZ      PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Produktkosten in Euro (P1)
                  20 PR-SUMMEN-EUR            PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Produktkosten in % (P2)
                  20 PR-SUMMEN-PROZ           PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Gesamtkosten in Euro (Q1)
                  20 GSMT-SUMME-KOSTEN-EUR    PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Gesamtsumme Gesamtkosten in % (Q2)
                  20 GSMT-SUMME-KOSTEN-PROZ   PIC  X(007).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Summe einmalige Kosten in EUR (R)
                  20 SONST-EINMAL-KOSTEN      PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Summe laufende Kosten in EUR (S)
                  20 SONST-LFD-KOSTEN         PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      *           Durchschnittliches Depotvolumen (T)
                  20 SONST-DDEPOT-VOL         PIC  X(014).
                  20 FILLER                   PIC  X(001) VALUE ';'.
      ***     VARIABLER TEIL (TABELLE MIT FINANZINSTRUMENTE EINES DEPOTS)
              15 KOSTENAUSWEIS-FINST-TABELLE.
                 20 FINST-TAB OCCURS 1 TO 150
                              DEPENDING ON FINST-ANZ.
      *             Bezeichnung des Fonds (G)
                    25 FINST-FONDSBEZ        PIC  X(060).
                    25 FILLER-1              PIC  X(001).
      *             ISIN (H)
                    25 FINST-ISIN            PIC  X(012).
                    25 FILLER-2              PIC  X(001).
      *             WKN (H1)
                    25 FINST-WKN             PIC  X(006).
                    25 FILLER-3              PIC  X(001).
      *             Dienstleistungskosten in EUR (I)
                    25 DL-KOSTEN-EUR         PIC  X(014).
                    25 FILLER-4              PIC  X(001).
      *             Dienstleistungskosten in % (I1)
                    25 DL-KOSTEN-PROZ        PIC  X(007).
                    25 FILLER-5              PIC  X(001).
      *             Provision aus Dienstleistungskosten in EUR (J)
                    25 DL-PROVISIONEN-EUR    PIC  X(014).
                    25 FILLER-6              PIC  X(001).
      *             Provision aus Dienstleistungskosten in % (J1)
                    25 DL-PROVISIONEN-PROZ   PIC  X(007).
                    25 FILLER-7              PIC  X(001).
      *             Produktkosten in EUR (K)
                    25 PR-KOSTEN-EUR         PIC  X(014).
                    25 FILLER-8              PIC  X(001).
      *             Produktkosten in % (K1)
                    25 PR-KOSTEN-PROZ        PIC  X(007).
                    25 FILLER-9              PIC  X(001).
      *             Gesamtkosten pro Finanzinstrument in EUR (L)
                    25 GSMT-KOSTEN-EUR       PIC  X(014).
                    25 FILLER-10             PIC  X(001).
      *             Gesamtkosten pro Finanzinstrument in % (L1)
                    25 GSMT-KOSTEN-PROZ      PIC  X(007).
                    25 FILLER-11             PIC  X(001).

      *****************************************************************
      *---------------------- ENDE DRPA1956 --------------------------*
      *****************************************************************
