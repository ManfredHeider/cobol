      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VersionManager
      *            Datum      :  27.12.2019
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2019 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5172.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5172.CPv  $
      *
      *   Rev 2.1   Apr 01 2020 08:12:36   t13ex15
      *AMA00463: Bestandsdaten Copy (vor 2014)
      *
      *   Rev 2.0   Dec 27 2019 08:36:16   t13ex15
      *Archiverstellung
      *
      *---------------------------------------------------------------*
        **************************************************************
      *
      *     EINZELNACHWEIS DER SALDENVERÄNDERUNG AUS SST - NEU
      *
      *     SOURCE-NAME      :  DRMA5172
      * Historischer Name    :  DRMA3230
      *
      * Diese Copystrecke wird nicht generiert!
      * Sie dient zur Anzeige z.B. der Dateien
      * P130007.MA009812.G2002V00 bis 2013V00
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00463!01.04.20!2.1  ! Neuerstellung aus         ! S.Warz
      *         !        !     ! DRMA3230 (300 Byte)       !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
              05 DRMA5172.
              07 BUCHUNGSDATEN.
                 09 SALDENDATEN.
                    11 VBNR             PIC X(10).
                    11 FACH-KENNUNG     PIC X(10).
                    11 DV-KENNUNG       PIC X(10).
                    11 BUCHDAT          PIC X(10).
                    11 KTO-KZN-1        PIC 9.
                    11 GKTO-KZN-1       PIC 999.
                    11 BNGBETR-1        PIC S9(11)V99  PACKED-DECIMAL.
                    11 KTO-KZN-2        PIC X.
                    11 GKTO-KZN-2       PIC 999.
                    11 BNGBETR-2        PIC S9(11)V99  PACKED-DECIMAL.
                 09 BUCHTEXT            PIC X(40).
                 09 SORT-FELDER.
                    11 GESKZ            PIC 99.
                    11 PRODSCHL         PIC 999.
                    11 VERTRAGS-ID      PIC X(15).
                    11 TARIFKZ          PIC 99.
 
010420        07 SST-DATEN-FILLER.
010420           09 SST-DATEN-200       PIC X(200).
010420
              07 DAISY-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 DAISY-DATEN.
                    11 DRMA3218.
                       13 VN            PIC X(20).
                       13 BEWHKFT       PIC X.
                       13 BEWART        PIC 9(6).
                       13 BEW-DATUM.
                          15 BT-JRH     PIC 99.
                          15 BT.
                             17 BT-JHR  PIC 99.
                             17 BT-MON  PIC 99.
                          15 BT-TAG     PIC 99.
                       13 AVERM.
                          15 AVERM1-9   PIC 9(9).
                          15 PRZ        PIC X.
                       13 TARGR         PIC XXX.
                       13 TARIF         PIC XXX.
                       13 PRODART       PIC 9.
                       13 PRODSCHL      PIC 999.
                       13 HZRBEG.
                          15 HZRBEG-JAHR
                                        PIC 9(4).
                          15 HZRBEG-JAHR-R REDEFINES HZRBEG-JAHR.
                             17 HZRBEG-JRH
                                        PIC 99.
                             17 HZRBEG-JHR
                                        PIC 99.
                          15 HZRBEG-MON PIC 99.
                          15 HZRBEG-TAG PIC 99.
                       13 HZREND.
                          15 HZREND-JAHR
                                        PIC 9(4).
                          15 HZREND-JAHR-R REDEFINES HZREND-JAHR.
                             17 HZREND-JRH
                                        PIC 99.
                             17 HZREND-JHR
                                        PIC 99.
                          15 HZREND-MON PIC 99.
                          15 HZREND-TAG PIC 99.
                       13 PRO-RATA-SU   PIC S9(9)V99   PACKED-DECIMAL.
                       13 AE            PIC S9(10)V9   PACKED-DECIMAL.
                       13 PE-MANR       PIC X(10).
                       13 PRSATZ        PIC S999V99    PACKED-DECIMAL.
                       13 DDRKZ         PIC X.
                       13 VM-BETEIL-KZN PIC X.
                       13 EIGEN-KZN     PIC X.
                       13 PR-PFL-SU     PIC S9(9)V99   PACKED-DECIMAL.
                    11 NOTE.
                       13 NOTE-NR       PIC X.
                       13 AMPROTZ-SATZ  PIC XX.
                    11 FAELLIG          PIC 9(8).
                    11 BESTSCHL         PIC XX.
                    11 PDPT-ID          PIC S9(15)     PACKED-DECIMAL.
                    11 ORD-VNR          PIC X(15).
                    11 REIHENFOLGE-NR   PIC 999.
                    11 HAFTZEIT-KZN     PIC XX.
                    11 PDPT-FIRST-INS   PIC X(26).
                    11 FILLER           OCCURS 5 TIMES
                                        PIC X.
                 09 FILLER              PIC X(23).
010420
010420        07 SST-DATEN-FILLER-EINZEL REDEFINES SST-DATEN-FILLER.
010420           09 SST-DATEN           OCCURS 200 TIMES
010420                                  PIC X.
 
              07 PROVIS-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 PROVIS-DATEN.
                    11 DRMA3218.
                       13 VN            PIC X(20).
                       13 BEWHKFT       PIC X.
                       13 BEWART        PIC 9(6).
                       13 BEW-DATUM.
                          15 BT-JRH     PIC 99.
                          15 BT.
                             17 BT-JHR  PIC 99.
                             17 BT-MON  PIC 99.
                          15 BT-TAG     PIC 99.
                       13 AVERM.
                          15 AVERM1-9   PIC 9(9).
                          15 PRZ        PIC X.
                       13 TARGR         PIC XXX.
                       13 TARIF         PIC XXX.
                       13 PRODART       PIC 9.
                       13 PRODSCHL      PIC 999.
                       13 HZRBEG.
                          15 HZRBEG-JAHR
                                        PIC 9(4).
                          15 HZRBEG-JAHR-R REDEFINES HZRBEG-JAHR.
                             17 HZRBEG-JRH
                                        PIC 99.
                             17 HZRBEG-JHR
                                        PIC 99.
                          15 HZRBEG-MON PIC 99.
                          15 HZRBEG-TAG PIC 99.
                       13 HZREND.
                          15 HZREND-JAHR
                                        PIC 9(4).
                          15 HZREND-JAHR-R REDEFINES HZREND-JAHR.
                             17 HZREND-JRH
                                        PIC 99.
                             17 HZREND-JHR
                                        PIC 99.
                          15 HZREND-MON PIC 99.
                          15 HZREND-TAG PIC 99.
                       13 PRO-RATA-SU   PIC S9(9)V99   PACKED-DECIMAL.
                       13 AE            PIC S9(10)V9   PACKED-DECIMAL.
                       13 PE-MANR       PIC X(10).
                       13 PRSATZ        PIC S999V99    PACKED-DECIMAL.
                       13 DDRKZ         PIC X.
                       13 VM-BETEIL-KZN PIC X.
                       13 EIGEN-KZN     PIC X.
                       13 PR-PFL-SU     PIC S9(9)V99   PACKED-DECIMAL.
                    11 RWERTSU          PIC S9(9)      PACKED-DECIMAL.
                    11 BUZGENJJ         PIC 99.
                    11 TARIFBEZ         PIC X(12).
                    11 TARIFSCHL        PIC S9(5)      PACKED-DECIMAL.
                    11 DYNAMIK-KZN      PIC X.
                    11 ZUSCHUSS-KZN     PIC X.
                    11 SONDERBER        PIC XX.
                    11 BEARBTERM        PIC S9(15)     PACKED-DECIMAL.
                    11 VFM-KZN          PIC X.
                    11 BEGR-BWERTSU     PIC X.
                    11 BAV-KZN          PIC X.
                    11 EURO-KZN         PIC X.
                    11 SONDER-KZN       PIC X.
                    11 TARIFSCHL-GRP    PIC S999       PACKED-DECIMAL.
                    11 HT-TARIFSCHL     PIC S9(5)      PACKED-DECIMAL.
                    11 HT-TARIFSCHL-GRP PIC S999       PACKED-DECIMAL.
                    11 VT               PIC X(17).
                    11 DYNAMISIERBAR    PIC X.
                    11 VVG-KZN          PIC X.
                    11 NEUSTOKZ         PIC X.
                    11 ZAHLW            PIC 99.
                    11 PROVSCHL         PIC X(5).
                 09 FILLER              PIC X(22).
              07 KOMPAS-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 KOMPAS-DATEN.
                    11 DRMA3218.
                       13 VN            PIC X(20).
                       13 BEWHKFT       PIC X.
                       13 BEWART        PIC 9(6).
                       13 BEW-DATUM.
                          15 BT-JRH     PIC 99.
                          15 BT.
                             17 BT-JHR  PIC 99.
                             17 BT-MON  PIC 99.
                          15 BT-TAG     PIC 99.
                       13 AVERM.
                          15 AVERM1-9   PIC 9(9).
                          15 PRZ        PIC X.
                       13 TARGR         PIC XXX.
                       13 TARIF         PIC XXX.
                       13 PRODART       PIC 9.
                       13 PRODSCHL      PIC 999.
                       13 HZRBEG.
                          15 HZRBEG-JAHR
                                        PIC 9(4).
                          15 HZRBEG-JAHR-R REDEFINES HZRBEG-JAHR.
                             17 HZRBEG-JRH
                                        PIC 99.
                             17 HZRBEG-JHR
                                        PIC 99.
                          15 HZRBEG-MON PIC 99.
                          15 HZRBEG-TAG PIC 99.
                       13 HZREND.
                          15 HZREND-JAHR
                                        PIC 9(4).
                          15 HZREND-JAHR-R REDEFINES HZREND-JAHR.
                             17 HZREND-JRH
                                        PIC 99.
                             17 HZREND-JHR
                                        PIC 99.
                          15 HZREND-MON PIC 99.
                          15 HZREND-TAG PIC 99.
                       13 PRO-RATA-SU   PIC S9(9)V99   PACKED-DECIMAL.
                       13 AE            PIC S9(10)V9   PACKED-DECIMAL.
                       13 PE-MANR       PIC X(10).
                       13 PRSATZ        PIC S999V99    PACKED-DECIMAL.
                       13 DDRKZ         PIC X.
                       13 VM-BETEIL-KZN PIC X.
                       13 EIGEN-KZN     PIC X.
                       13 PR-PFL-SU     PIC S9(9)V99   PACKED-DECIMAL.
                    11 RWERTSU          PIC S9(9)      PACKED-DECIMAL.
                    11 BUZGENJJ         PIC 99.
                    11 TARIFBEZ         PIC X(12).
                    11 TARIFSCHL        PIC 999.
                    11 DYNAMIK-KZN      PIC X.
                    11 ZUSCHUSS-KZN     PIC X.
                    11 SONDERBER        PIC XX.
                    11 BEARBTERM        PIC S9(15)     PACKED-DECIMAL.
                    11 VFM-KZN          PIC X.
                    11 BEGR-BWERTSU     PIC X.
                    11 BAV-KZN          PIC X.
                    11 EURO-KZN         PIC X.
                    11 SONDER-KZN       PIC X.
                    11 TARIFSCHL-GRP    PIC 99.
                    11 HT-TARIFSCHL     PIC 999.
                    11 HT-TARIFSCHL-GRP PIC 99.
                    11 VT               PIC X(17).
                    11 VERTR-T-VERSION  PIC S9(15)     PACKED-DECIMAL.
                    11 DYNAMISIERBAR    PIC X.
                    11 VVG-KZN          PIC X.
                    11 NEUSTOKZ         PIC X.
                 09 FILLER              PIC X(21).
              07 ERFPROV-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 ERFPROV-DATEN.
                    11 AE-MONATS-DS     PIC S9(7)V99   PACKED-DECIMAL.
                    11 BRUTTO-BETRAG-EP PIC S9(7)V99   PACKED-DECIMAL.
                    11 EQF              PIC S999V99    PACKED-DECIMAL.
                    11 FAKTOR           PIC S9V99      PACKED-DECIMAL.
                 09 FILLER              PIC X(185).
              07 FIBU-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 FIBU-DATEN.
                    11 KZN-SONSTBUCH    PIC 9(8).
                 09 FILLER              PIC X(192).
              07 AUFBBON-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 AUFBBON-DATEN.
                    11 BETREUTER        PIC 9(7).
                    11 EQF              PIC S999V99    PACKED-DECIMAL.
                    11 AE-MONATS-DS     PIC S9(9)V9    PACKED-DECIMAL.
                    11 DATUM            PIC X(10).
                 09 FILLER              PIC X(174).
              07 LV-BONUS-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 LV-BONUS-DATEN.
                    11 EH-AP-NT-EG      PIC S9(9)V9    PACKED-DECIMAL.
                    11 EH-AP-NT-EG-LV   PIC S9(9)V9    PACKED-DECIMAL.
                    11 EH-AP-NT-EG-LV-VORJAHR
                                        PIC S9(9)V9    PACKED-DECIMAL.
                 09 FILLER              PIC X(182).
              07 EXGRBON-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 EXGRBON-DATEN.
                    11 AE-MONATS-DS     PIC S9(10)V9   PACKED-DECIMAL.
                    11 BRUTTO-BETRAG-EXB
                                        PIC S9(9)V99   PACKED-DECIMAL.
                    11 EQF              PIC S999V99    PACKED-DECIMAL.
                    11 FAKTOR           PIC S9V99      PACKED-DECIMAL.
                 09 FILLER              PIC X(183).
              07 ZUSATZBON-DATEN-FILLER REDEFINES SST-DATEN-FILLER.
                 09 ZUSATZBON-DATEN.
                    11 AE-MONATS-DS     PIC S9(10)V9   PACKED-DECIMAL.
                    11 BRUTTO-BETRAG-ZB PIC S9(9)V99   PACKED-DECIMAL.
                    11 EQF              PIC S999V99    PACKED-DECIMAL.
                    11 FAKTOR           PIC S9V99      PACKED-DECIMAL.
                 09 FILLER              PIC X(183).

