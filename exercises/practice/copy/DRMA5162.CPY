      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.10.2017
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2017 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5162.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5162.CPv  $
      *
      *   Rev 2.1   Nov 01 2017 14:11:36   t13ac18
      *AMA00360: 3. Statistik-Kategorie Folge Neu - Copies PGM für 
      *Buchauszug
      *
      *   Rev 2.0   Oct 27 2017 13:29:40   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     EINZELNACHWEIS DER SALDENVERÄNDERUNG AUS SST - NEU
      *
      *     SOURCE-NAME      :  DRMA5162
      * Historischer Name    :  DRMA3230
      *
      * Diese Copystrecke wird nicht generiert!
      * Sie dient zur Anzeige z.B. der Dateien
      * P130007.MA009812.G2014V00 bis 2016V00 vor der Umstellung
      * von PRODART-VG auf 2stellig und DGV auf 5stellig
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00360!27.10.17!2.1  ! Neuerstellung aus         ! D.Wilhelm
      *         !        !     ! DRMA3230 Version 2.10     !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5162.
              07 DRMA5083.
                 09 BUCHUNGSDATEN.
                    11 SALDENDATEN.
                       13 VBNR          pic 9(9).
                       13 FACH-KENNUNG  pic X(10).
                       13 DV-KENNUNG    pic X(10).
                       13 BUCHDAT       pic X(10).
                       13 KTO-KZN-1     pic 9.
                       13 BNGBETR-1     pic S9(9)V99   packed-decimal.
                       13 KTO-KZN-2     pic X.
                       13 BNGBETR-2     pic S9(9)V99   packed-decimal.
                    11 SORT-FELDER.
                       13 PG            pic 99.
                       13 PRODSCHL-SORT pic 999.
                       13 VERTRAGS-ID-SORT
                                        pic X(15).
                       13 TARIF-KZN     pic 99.
                 09 SST-DATEN           pic X(400).
                 09 DAISY-DATEN redefines SST-DATEN.
                    11 DGMA0102.
                       13 VN            pic X(20).
                       13 VT            pic X(20).
                       13 DGV           pic 9(4).
                       13 AVERM         pic 9(9).
                       13 PRODART-VG    pic 9.
                       13 HZRBEG        pic X(10).
                       13 HZREND        pic X(10).
                       13 PRO-RATA-SU   pic S9(9)V99   packed-decimal.
                       13 VM-BETEIL-KZN pic X.
                       13 PR-PFL-SU     pic S9(9)V99   packed-decimal.
                       13 PR-PFL-SU-ABR pic S9(9)V99   packed-decimal.
                       13 FAELLIG-VON   pic X(10).
                       13 FAELLIG-BIS   pic X(10).
                       13 PDPT-ID       pic S9(15)     packed-decimal.
                       13 VERTRAGS-ID   pic X(15).
                       13 REIHENFOLGE-NR
                                        pic 999.
                       13 HAFTZEIT-KZN  pic XX.
                       13 BEW-ERSTELL-ZPKT
                                        pic X(26).
                       13 DYN-KZN1      pic 9.
                       13 DYN-KZN2      pic 9.
                       13 PRO-RATA-ZAEHLER
                                        pic 999.
                       13 PRO-RATA-NENNER
                                        pic 999.
                       13 PRODSCHL      pic 999.
                       13 PROVSCHL      pic X(5).
                       13 RWERTSU       pic S9(9)V99   packed-decimal.
                       13 BEWSCHL       pic X(7).
                       13 PGV           pic 9(4).
                       13 PG-RECH-NR    pic X(12).
                       13 ERWERB-FOLGE-KZN
                                        pic X(8).
                       13 DGMA5007.
                          15 UST-BERECH-KZN
                                        pic X.
                          15 UST-SATZ   pic S999V99    packed-decimal.
                          15 PROV-AUS-RUECK-UST
                                        pic S9(9)V99   packed-decimal.
                          15 PROV-AUS-RUECK
                                        pic S9(9)V99   packed-decimal.
                          15 PROV-AUS   pic S9(9)V99   packed-decimal.
                          15 PROV-RUECK pic S9(9)V99   packed-decimal.
                          15 PROV-UST   pic S9(9)V99   packed-decimal.
                       13 DGMA5008.
                          15 UST-KORREKTUR-KZN
                                        pic X.
                          15 KORREKTUR-ZIEL-STATUS
                                        pic X.
                          15 ABR-NR-KORR
                                        pic X(15).
                          15 STEUERJAHR pic X(4).
                    11 BAV-KZN          pic X.
                    11 VFM-KZN          pic X.
                    11 EMP-TYP          pic 99.
                    11 PRSATZ           pic S999V99    packed-decimal.
                    11 PE-MANR          pic 9(9).
                    11 PE-AE            pic S9(10)V9   packed-decimal.
                    11 EIGEN-KZN        pic X.
                    11 VSP-FLEXN-RELEVANZ
                                        pic X.
                    11 VSP-FLEXN-FOLGE-KZ
                                        pic X.
                    11 VSP-PRODUKT-KOMBI-NR
                                        pic 9(5).
                    11 VSP-PRODUKT-KOMBI-BEZ
                                        pic XXX.
                    11 VSP-FLEXN-BERECHTIGUNG
                                        pic X.
                 09 DAISY-PG-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 ERWERB-FOLGE-KZN pic X(8).
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-RUECK-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-RUECK
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-RUECK    pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                 09 FIBU-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 FIBU-SORT-ID     pic 9(4).
                    11 FIBU-SORT-LFDNR  pic 9(4).
                    11 UST-PFLICHTIG-KZN
                                        pic X.
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-RUECK-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-RUECK
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-RUECK    pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                    11 DGMA5008.
                       13 UST-KORREKTUR-KZN
                                        pic X.
                       13 KORREKTUR-ZIEL-STATUS
                                        pic X.
                       13 ABR-NR-KORR   pic X(15).
                       13 STEUERJAHR    pic X(4).
                    11 REFERENZ-NR      pic X(16).
                    11 KNR-VERARB-TYP   pic S9(4)      binary.
                 09 SONDLST-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
                    11 UST-PFLICHTIG-KZN
                                        pic X.
                    11 DGMA5007.
                       13 UST-BERECH-KZN
                                        pic X.
                       13 UST-SATZ      pic S999V99    packed-decimal.
                       13 PROV-AUS-RUECK-UST
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS-RUECK
                                        pic S9(9)V99   packed-decimal.
                       13 PROV-AUS      pic S9(9)V99   packed-decimal.
                       13 PROV-RUECK    pic S9(9)V99   packed-decimal.
                       13 PROV-UST      pic S9(9)V99   packed-decimal.
                 09 SONSTIGE-DATEN redefines SST-DATEN.
                    11 BUCHTEXT         pic X(92).
      *
      **************************************************************

