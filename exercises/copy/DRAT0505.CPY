      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRAT0505.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRAT0505.CPv  $
      *
      *   Rev 2.1   Jun 28 2013 14:59:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     KL_LVV_BEWEGUNG/EMPF
      *     RDz
      *     SOURCE-NAME      :  DRAT0505
      *     DMR-NAME         :  DG-KL6636
      *
      *     AUTOR            :  BURKARD, CLAUDIA
      *     GENERIERUNG      :  05.06.2012/09:46:55
      *     LETZTE AENDERUNG :
      *
      **************************************************************
      *
      *
           05 LV1-OM690102.
              07 LV1-OM690102-A.
                 09 LV1-LAENGE          PIC S9(4)      BINARY.
                 09 LV1-LV1KEY.
                    11 LV1-BEARBTERM.
                       13 LV1-BEARB-DATUM
                                        PIC 9(8).
                       13 LV1-BEARB-DATUM-RED REDEFINES LV1-BEARB-DATUM.
                          15 LV1-BEARB-DATUM-JRH
                                        PIC 99.
                          15 LV1-BEARB-DATUM-JHR
                                        PIC 99.
                          15 LV1-BEARB-DATUM-MON
                                        PIC 99.
                          15 LV1-BEARB-DATUM-TAG
                                        PIC 99.
                       13 LV1-BEARB-ZEITNR
                                        PIC 9(7).
                       13 FILLER REDEFINES LV1-BEARB-ZEITNR.
                          15 LV1-BEARB-HHMMSS
                                        PIC 9(6).
                          15 LV1-BEARB-ZSEK
                                        PIC 9.
                 09 LV1-EKRIT.
                    11 LV1-EKRIT-PGM    PIC X(8).
                    11 LV1-EKRIT-PNR-FILLER REDEFINES LV1-EKRIT-PGM.
                       13 LV1-EKRIT-PNR PIC 9(7).
                       13 FILLER        PIC X.
                    11 LV1-EKRIT-DATUM  PIC 9(8).
                    11 LV1-EKRIT-DATUM-R REDEFINES LV1-EKRIT-DATUM.
                       13 LV1-JRH       PIC 99.
                       13 LV1-JHR       PIC 99.
                       13 LV1-MON       PIC 99.
                       13 LV1-TAG       PIC 99.
                    11 LV1-EKRIT-ZEIT   PIC 9(7).
                    11 LV1-EKRIT-ZEIT-R REDEFINES LV1-EKRIT-ZEIT.
                       13 LV1-STD       PIC 99.
                       13 LV1-MIN       PIC 99.
                       13 LV1-SEK       PIC 99.
                       13 LV1-ZSEK      PIC 9.
                 09 LV1-VERWEIS.
                    11 LV1-VERWEIS-ART  PIC X.
                    11 LV1-VERW-DATZEIT.
                       13 LV1-VERW-DATUM
                                        PIC 9(8).
                       13 LV1-VERW-ZEIT PIC 9(7).
                 09 LV1-UMWAND.
                    11 FILLER           PIC X.
                    11 LV1-UMW-GESAMT   PIC 9(15).
                    11 LV1-UMWAND-NUMMER REDEFINES LV1-UMW-GESAMT.
                       13 LV1-UMW-VERMGES
                                        PIC 99.
                       13 LV1-UMW-VSNR  PIC 9(11).
                       13 LV1-UMW-TARIFKZ
                                        PIC 99.
                    11 LV1-UMWAND-DATUM REDEFINES LV1-UMW-GESAMT.
                       13 LV1-UMW-BEARB-DATUM
                                        PIC 9(8).
                       13 LV1-UMW-BEARB-ZEITNR
                                        PIC 9(7).
                 09 LV1-BEWSCHL.
                    11 LV1-BEWHKFT      PIC X.
                    11 LV1-BEWART       PIC 9(6).
                    11 LV1-BEW-DATUM    PIC 9(8).
                    11 LV1-BEW-DATUM-R REDEFINES LV1-BEW-DATUM.
                       13 LV1-BT-JRH    PIC 99.
                       13 LV1-BT.
                          15 LV1-BT-JHR PIC 99.
                          15 LV1-BT-MON PIC 99.
                       13 LV1-BT-TAG    PIC 99.
                    11 LV1-BEW-DATUM-R REDEFINES LV1-BEW-DATUM.
                       13 LV1-BT-JAHR   PIC 9(4).
                       13 FILLER        PIC X(4).
                    11 LV1-TECH-BEW-DATUM
                                        PIC 9(8).
                    11 LV1-TECH-BEW-DATUM-R1 REDEFINES
                LV1-TECH-BEW-DATUM.
                       13 LV1-TT-JRH    PIC 99.
                       13 LV1-TT.
                          15 LV1-TT-JHR PIC 99.
                          15 LV1-TT-MON PIC 99.
                       13 LV1-TT-TAG    PIC 99.
                    11 LV1-TECH-BEW-DATUM-R2 REDEFINES
                LV1-TECH-BEW-DATUM.
                       13 LV1-TT-JAHR   PIC 9(4).
                       13 FILLER        PIC 9(4).
                 09 LV1-PROVSCHL        PIC X(5).
                 09 LV1-PROVSCHL-R REDEFINES LV1-PROVSCHL.
                    11 LV1-SCHLART      PIC X.
                    11 LV1-SCHLNR       PIC 999.
                    11 LV1-SCHLUNR      PIC X.
                 09 LV1-PROVSCHL-R REDEFINES LV1-PROVSCHL.
                    11 FILLER           PIC X.
                    11 LV1-SCHLNR4      PIC 9(4).
                 09 LV1-SCHLNR-1-3-FILLER REDEFINES LV1-PROVSCHL.
                    11 LV1-SCHLNR-1-3   PIC XXX.
                    11 FILLER           PIC XX.
                 09 LV1-EINR-DATUM.
                    11 LV1-EINR-JAHR    PIC 9(4).
                    11 LV1-EINR-JAHR-R REDEFINES LV1-EINR-JAHR.
                       13 LV1-EINR-JRH  PIC 99.
                       13 LV1-EINR-JHR  PIC 99.
                    11 LV1-EINR-MON     PIC 99.
                    11 LV1-EINR-TAG     PIC 99.
                 09 LV1-ANTRAGS-DATUM.
                    11 LV1-ANTRAGS-JAHR PIC 9(4).
                    11 LV1-ANTRAGS-JAHR-R REDEFINES LV1-ANTRAGS-JAHR.
                       13 LV1-ANTRAGS-JRH
                                        PIC 99.
                       13 LV1-ANTRAGS-JHR
                                        PIC 99.
                    11 LV1-ANTRAGS-MON  PIC 99.
                    11 LV1-ANTRAGS-TAG  PIC 99.
                 09 LV1-INKST-DATUM.
                    11 LV1-INK-JRH      PIC 99.
                    11 LV1-INKST.
                       13 LV1-INK-JHR   PIC 99.
                       13 LV1-INK-MON   PIC 99.
                    11 LV1-INK-TAG      PIC 99.
                 09 LV1-INKST-DATUM-R REDEFINES LV1-INKST-DATUM.
                    11 LV1-INK-JAHR     PIC 9(4).
                    11 FILLER           PIC 9(4).
                 09 LV1-IAART           PIC 99.
                 09 LV1-VERWST.
                    11 LV1-VERW-ORGGS   PIC 999.
                    11 LV1-V-ORGGS-R REDEFINES LV1-VERW-ORGGS.
                       13 LV1-VERW-ORG  PIC 9.
                       13 LV1-VERW-GS   PIC 99.
                    11 LV1-VERW-AGT     PIC 9(4).
                 09 LV1-VERWST-R REDEFINES LV1-VERWST.
                    11 FILLER           PIC 9.
                    11 LV1-VERW-AD-STELLE
                                        PIC 9(6).
                 09 LV1-AVERM           PIC X(10).
                 09 LV1-AVERM-R REDEFINES LV1-AVERM.
                    11 LV1-VERM-AD-STELLE
                                        PIC 9(6).
                    11 LV1-VERM-AD-ST-VERS-BEG
                                        PIC S9(9)      BINARY.
                 09 LV1-TARGR           PIC 999.
                 09 LV1-TARIF           PIC 999.
                 09 LV1-BUZTAR          PIC 9(4).
                 09 LV1-BUZGENJJ        PIC 99.
                 09 LV1-WRGKZN          PIC XXX.
                 09 LV1-GEVO-ART        PIC 99.
                 09 LV1-GES-BTG         PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-BTGZUST.
                    11 LV1-BTGSCHL      PIC 9.
                    11 LV1-BTG-DATUM.
                       13 LV1-BTG-JRH   PIC 99.
                       13 LV1-BTGDAT.
                          15 LV1-BTG-JHR
                                        PIC 99.
                          15 LV1-BTG-MON
                                        PIC 99.
                       13 LV1-BTG-TAG   PIC 99.
                    11 LV1-BTG-DATUM-R REDEFINES LV1-BTG-DATUM.
                       13 LV1-BTG-JAHR  PIC 9(4).
                       13 FILLER        PIC 9(4).
                 09 LV1-RATENBTG        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-REST-BTG        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-ZAHLW           PIC 99.
                 09 LV1-DDRKZ           PIC X.
                 09 LV1-OPTIONSKZ       PIC X.
                 09 LV1-VS-BETRAG       PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-VSR REDEFINES LV1-VS-BETRAG
                                        PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-BEFRENTE REDEFINES LV1-VS-BETRAG
                                        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-BARRENTE REDEFINES LV1-VS-BETRAG
                                        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-BUZRENTE REDEFINES LV1-VS-BETRAG
                                        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-BTG-BEZ         PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-BARPROZ-SATZ    PIC 99.
                 09 LV1-EINALTER1       PIC 9(4)       BINARY.
                 09 LV1-XVP1 REDEFINES LV1-EINALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BEFX1 REDEFINES LV1-EINALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BARX1 REDEFINES LV1-EINALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BUZX1 REDEFINES LV1-EINALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-HT-EINALTER1    PIC 9(4)       BINARY.
                 09 LV1-HAFTZ-BER       PIC XXX.
                 09 LV1-ENDALTER1       PIC 9(4)       BINARY.
                 09 LV1-YVP1 REDEFINES LV1-ENDALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BEFY1 REDEFINES LV1-ENDALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BARY1 REDEFINES LV1-ENDALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-BUZY1 REDEFINES LV1-ENDALTER1
                                        PIC 9(4)       BINARY.
                 09 LV1-GEWVA           PIC 9.
                 09 LV1-BESVV           PIC 9.
                 09 LV1-EINRUHRZEIT-NEU PIC S9(13)     PACKED-DECIMAL.
                 09 LV1-EINRUHRZEIT-R REDEFINES LV1-EINRUHRZEIT-NEU.
                    11 LV1-EINRUHRZEIT  PIC S9(8)      PACKED-DECIMAL.
                    11 LV1-EINRNR       PIC S9         PACKED-DECIMAL.
                    11 FILLER           PIC X.
                 09 LV1-TZVTF           PIC 99.
                 09 LV1-DAUER-BEZ       PIC S9(4)      BINARY.
                 09 LV1-REST-DAUER      PIC S9(4)      BINARY.
                 09 LV1-BZ-DAUER-BEZ    PIC S9(4)      BINARY.
                 09 LV1-BEG-DATUM.
                    11 LV1-BEG-JRH      PIC 99.
                    11 LV1-BEGINN.
                       13 LV1-BEG-JHR   PIC 99.
                       13 LV1-BEG-MON   PIC 99.
                    11 LV1-BEG-TAG      PIC 99.
                 09 LV1-BEG-DATUM-R REDEFINES LV1-BEG-DATUM.
                    11 LV1-BEG-JAHR     PIC 9(4).
                    11 FILLER           PIC 9(4).
                 09 LV1-ABL-DATUM.
                    11 LV1-ABL-JRH      PIC 99.
                    11 LV1-ABLAUF.
                       13 LV1-ABL-JHR   PIC 99.
                       13 LV1-ABL-MON   PIC 99.
                    11 LV1-ABL-TAG      PIC 99.
                 09 LV1-ABL-DATUM-R REDEFINES LV1-ABL-DATUM.
                    11 LV1-ABL-JAHR     PIC 9(4).
                    11 FILLER           PIC 9(4).
                 09 LV1-BZ-ABL-DATUM.
                    11 LV1-BZ-ABLDAT-JAHR
                                        PIC 9(4).
                    11 LV1-BZ-ABLDAT-JAHR-R REDEFINES
                LV1-BZ-ABLDAT-JAHR.
                       13 LV1-BZ-ABLDAT-JRH
                                        PIC 99.
                       13 LV1-BZ-ABLDAT-JHR
                                        PIC 99.
                    11 LV1-BZ-ABLDAT-MON
                                        PIC 99.
                    11 LV1-BZ-ABLDAT-TAG
                                        PIC 99.
                 09 LV1-DAUER           PIC S9(4)      BINARY.
                 09 LV1-HT-DAUER        PIC S9(4)      BINARY.
                 09 LV1-BZ-DAUER        PIC S9(4)      BINARY.
                 09 LV1-HT-BZ-DAUER     PIC S9(4)      BINARY.
                 09 LV1-JAHRESBTG       PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-PROV-DATUM      PIC 9(8).
                 09 LV1-PROV-DATUM-R REDEFINES LV1-PROV-DATUM.
                    11 LV1-PROVDAT-JAHR PIC 9(4).
                    11 LV1-PROVDAT-JAHR-R REDEFINES LV1-PROVDAT-JAHR.
                       13 LV1-PROVDAT-JRH
                                        PIC 99.
                       13 LV1-PROVDAT-JHR
                                        PIC 99.
                    11 LV1-PROVDAT-MON  PIC 99.
                    11 LV1-PROVDAT-TAG  PIC 99.
                 09 LV1-BUCHDAT         PIC 9(8).
                 09 LV1-BUCHDAT-R1 REDEFINES LV1-BUCHDAT.
                    11 LV1-BUCHDAT-JRH  PIC 99.
                    11 LV1-BUCHDAT-RED.
                       13 LV1-BUCHDAT-JHR
                                        PIC 99.
                       13 LV1-BUCHDAT-MON
                                        PIC 99.
                       13 LV1-BUCHDAT-TAG
                                        PIC 99.
                 09 LV1-BUCHDAT-R2 REDEFINES LV1-BUCHDAT.
                    11 LV1-BUCHDAT-JAHR PIC 9(4).
                    11 FILLER           PIC X(4).
                 09 LV1-LEISTKZ         PIC 9.
                 09 LV1-FREIKZ          PIC X.
                 09 LV1-DYNKZ1          PIC 9.
                 09 LV1-DYNKZ2          PIC 9.
                 09 LV1-PRODART         PIC 9.
                 09 LV1-PRODSCHL        PIC S999       PACKED-DECIMAL.
                 09 LV1-TARIFSCHL-ERW.
                    11 LV1-TARIFSCHL-GRP
                                        PIC S999       PACKED-DECIMAL.
                    11 LV1-TARIFSCHL    PIC S9(5)      PACKED-DECIMAL.
                 09 LV1-GRUNDPROV       PIC S999       PACKED-DECIMAL.
                 09 LV1-BER-GRUNDLAGE   PIC S9(9)V99   PACKED-DECIMAL.
                 09 LV1-BWERTSU         PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-RWERTSU         PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-BWERTBTG        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-RWERTBTG        PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-HAFTART         PIC XX.
                 09 LV1-HZRBEG.
                    11 LV1-HZRBEG-JAHR  PIC 9(4).
                    11 LV1-HZRBEG-JAHR-R REDEFINES LV1-HZRBEG-JAHR.
                       13 LV1-HZRBEG-JRH
                                        PIC 99.
                       13 LV1-HZRBEG-JHR
                                        PIC 99.
                    11 LV1-HZRBEG-MON   PIC 99.
                    11 LV1-HZRBEG-TAG   PIC 99.
                 09 LV1-HZREND.
                    11 LV1-HZREND-JAHR  PIC 9(4).
                    11 LV1-HZREND-JAHR-R REDEFINES LV1-HZREND-JAHR.
                       13 LV1-HZREND-JRH
                                        PIC 99.
                       13 LV1-HZREND-JHR
                                        PIC 99.
                    11 LV1-HZREND-MON   PIC 99.
                    11 LV1-HZREND-TAG   PIC 99.
                 09 LV1-PRO-RATA-SU     PIC S9(9)V99   PACKED-DECIMAL.
                 09 LV1-PRO-RATA-BTG    PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-PRO-RATA-FAKT   PIC 9(6).
                 09 LV1-PRO-RATA-FAKT-R REDEFINES LV1-PRO-RATA-FAKT.
                    11 LV1-PRO-RATA-ZAEHLER
                                        PIC 999.
                    11 LV1-PRO-RATA-NENNER
                                        PIC 999.
                 09 LV1-PR-PFL-SU       PIC S9(9)V99   PACKED-DECIMAL.
                 09 LV1-PR-PFL-BTG      PIC S9(7)V99   PACKED-DECIMAL.
                 09 LV1-AE              PIC S9(6)V9    PACKED-DECIMAL.
                 09 LV1-BARKZ           PIC X.
                 09 LV1-RISIKOKZ        PIC 99.
                 09 LV1-MIGRKZ          PIC X.
                 09 LV1-ZRZVTF          PIC 9.
                 09 LV1-SAMNR           PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-RABSTUFE        PIC XXX.
                 09 LV1-BVKZ            PIC X.
                 09 LV1-REST-BZ-DAUER   PIC S9(4)      BINARY.
                 09 LV1-BFGKZ           PIC X.
                 09 LV1-UMWAND-ART      PIC 99.
                 09 LV1-PROVAUSL        PIC 99.
                 09 LV1-DEPOTKZ         PIC X.
                 09 LV1-BEGR-BWERTSU    PIC X.
                 09 LV1-PROVSCHL-HIST   PIC X(5).
                 09 LV1-INFOKEY         PIC XX.
                 09 LV1-NEUSTOKZ        PIC X.
                 09 LV1-EKRIT-PGM-HIST  PIC X(8).
                 09 LV1-EKRIT-PGM-HIST-RED REDEFINES LV1-EKRIT-PGM-HIST.
                    11 LV1-EKRIT-PNR-HIST
                                        PIC 9(7).
                    11 FILLER           PIC X.
                 09 LV1-EKRIT-DATUM-HIST
                                        PIC 9(8).
                 09 LV1-TARIFBEZ        PIC X(15).
                 09 LV1-BUCHDAT-VG      PIC S9(9)      PACKED-DECIMAL.
                 09 LV1-SONDERKZ        PIC X.
                 09 LV1-NBBS            PIC S9(9)V99   PACKED-DECIMAL.
                 09 LV1-LVART           PIC 99.
                 09 LV1-PROVVERARBKZN   PIC 99.
                 09 LV1-PROVVERDIENT    PIC 9(4).
                 09 LV1-PROVVERDIENT-R REDEFINES LV1-PROVVERDIENT.
                    11 LV1-PROVVERDIENT-ZAEHLER
                                        PIC 99.
                    11 LV1-PROVVERDIENT-NENNER
                                        PIC 99.
                 09 LV1-PROVBEG         PIC 9(8).
                 09 LV1-PROVBEG-RED REDEFINES LV1-PROVBEG.
                    11 LV1-PROVBEG-JAHR PIC 9(4).
                    11 LV1-PROVBEG-MON  PIC 99.
                    11 LV1-PROVBEG-TAG  PIC 99.
                 09 LV1-PROVABL         PIC 9(8).
                 09 LV1-PROVABL-RED REDEFINES LV1-PROVABL.
                    11 LV1-PROVABL-JAHR PIC 9(4).
                    11 LV1-PROVABL-MON  PIC 99.
                    11 LV1-PROVABL-TAG  PIC 99.
                 09 LV1-PROVABLBZ       PIC 9(8).
                 09 LV1-PROVABLBZ-RED REDEFINES LV1-PROVABLBZ.
                    11 LV1-PROVABLBZ-JAHR
                                        PIC 9(4).
                    11 LV1-PROVABLBZ-MON
                                        PIC 99.
                    11 LV1-PROVABLBZ-TAG
                                        PIC 99.
                 09 LV1-URSPRBEG        PIC 9(8).
                 09 LV1-AP-FAELLIG      PIC 9(8).
                 09 LV1-LAENDANTRDAT    PIC 9(8).
                 09 LV1-HISTBEGINN      PIC 9(8).
                 09 LV1-HISTUNR         PIC 999.
                 09 LV1-PAR10AKZN       PIC 99.
                 09 LV1-PROVKZN         PIC X.
                 09 LV1-GEBER           PIC 999.
                 09 LV1-HT-PRODSCHL     PIC S999       PACKED-DECIMAL.
                 09 LV1-STRUKTUR-KZN    PIC X.
                 09 LV1-UEBART          PIC 99.
                 09 LV1-SPLITT-FAKTOR   PIC S99V9(4)   PACKED-DECIMAL.
                 09 LV1-KONSORTNR       PIC S9(8)      PACKED-DECIMAL.
                 09 LV1-ORG-EINH        PIC X(4).
                 09 LV1-ORG-EINH-VERM   PIC X(4).
                 09 LV1-SPLITT-FAKTOR-V PIC S99V9(4)   PACKED-DECIMAL.
                 09 LV1-EINALTER2       PIC 9(4)       BINARY.
                 09 LV1-BAVFINART       PIC 99.
                 09 LV1-UEBERNAHME-STRUK
                                        PIC X.
                 09 LV1-KOMPAS-SPERR-KZN
                                        PIC X.
                 09 LV1-FEIV-MIG-KZN    PIC X.
                 09 LV1-PIF-AKTUELL     PIC X(10).
                 09 LV1-VVG-KZN         PIC X.
                 09 LV1-STRUK-HERK-KZN  PIC X.
                 09 LV1-PRODART-VG      PIC 9.
                 09 LV1-BUCHDAT-FEIV    PIC 9(8).
                 09 FILLER              PIC X(100).
                 09 LV1-ANZ-PE          PIC 99.
                 09 LV1-VERTEILKZ       PIC X.
                 09 LV1-PROVEMPF.
                    11 FILLER           OCCURS 98 TIMES
                                        PIC X(10).
                 09 LV1-PROVEMPF-DATEN REDEFINES LV1-PROVEMPF
                                        OCCURS 20 TIMES
                                        INDEXED LV1-PE-IND.
                    11 LV1-PE-MANR      PIC X(10).
                    11 LV1-PE-MANR-R REDEFINES LV1-PE-MANR.
                       13 LV1-PE-ORGGS  PIC 999.
                       13 LV1-FI REDEFINES LV1-PE-ORGGS.
                          15 LV1-PE-ORG PIC 9.
                          15 LV1-PE-GS  PIC 99.
                       13 LV1-PE-AGT    PIC 9(4).
                       13 LV1-PE-AV1    PIC 9.
                       13 LV1-PE-AV2    PIC 9.
                       13 LV1-PE-PRZ    PIC X.
                    11 LV1-PE-MANR-R2 REDEFINES LV1-PE-MANR.
                       13 LV1-PE-AD-STELLE
                                        PIC 9(6).
                       13 LV1-PE-AD-ST-VERS-BEGINN
                                        PIC S9(9)      BINARY.
                    11 LV1-PE-VERF      PIC X(8).
                    11 LV1-PE-VGSATZ    PIC S999V99    PACKED-DECIMAL.
                    11 LV1-PE-BEGRSATZ  PIC S999       PACKED-DECIMAL.
                    11 LV1-PE-DYNSATZ   PIC S999       PACKED-DECIMAL.
                    11 LV1-PE-HAFTZEIT  PIC XXX.
                    11 LV1-PE-PROVKZ    PIC X.
                    11 LV1-PE-PROVISION PIC S9(7)V99   PACKED-DECIMAL.
                    11 LV1-PE-AE        PIC S9(6)V9    PACKED-DECIMAL.
                    11 FILLER           PIC X(5).
                    11 LV1-PE-STATUS    PIC X.
                    11 FILLER           PIC X(5).
      *
      **************************************************************
