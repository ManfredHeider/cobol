      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5125.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5125.CPv  $
      *
      *   Rev 2.4   Nov 23 2017 10:40:22   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu SST Daisy
      *
      *   Rev 2.3   Mar 16 2017 13:15:02   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.2   Mar 21 2016 19:23:08   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.1   Jan 19 2016 12:03:48   t13ac14
      *AMA00218: USt: LUCY I - Nacharbeiten aus AMA00200
      *
      *   Rev 2.0   Dec 09 2015 14:19:14   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: DAISY MITARBEITER-BASIS-DATENSÄTZE
      *
      *     SOURCE-NAME      :  DRMA5125
      *
      *     GENERIERUNG      :  14.11.2017/13:18:04
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00218!09.12.15!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00278!04.03.16!2.2  ! STEUERJAHR EINGEFÜGT;     ! O.WILHELM
      *         !        !     ! DEMA0429: AUSPRÄGUNG VON  !
      *         !        !     !        02 IN 06 GEÄNDERT  !
      * AMA00297!21.02.17!2.3  ! FLEXN FELDER EINGEFÜGT    ! S.WARZ
      * AMA00360!07.11.17!2.4  ! DEMA0111 02 GEÄNDERT AUF  ! WARZ
      *         !        !     ! 05 FÜR DGV                !
      *         !        !     ! ÄNDERUNG DEMA0128         !
      *         !        !     ! ÄNDERUNG DEMA0160 EINBEH. !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5125.
              07 SATZ-KZ                pic X.
              07 PGEB-GES               pic 99.
              07 PROD-PROD-SCHL-SORT    pic 999.
              07 VNR-SORT               pic X(15).
              07 TARIF-KZ               pic 9(4).
              07 TARIF-BEZEICHNUNG      pic X(15).
              07 PROV-BETRAG-AUS        pic S9(9)V99.
              07 PROV-BETRAG-EINB       pic S9(9)V99.
              07 KUNDENNAME             pic X(20).
              07 VP-NAME                pic X(20).
              07 DGV                    pic 9(5).
              07 AVERM                  pic 9(7).
              07 PRODART                pic 99.
              07 HAFTZ-VON              pic X(10).
              07 HAFTZ-BIS              pic X(10).
              07 PRO-RATA-SUMME         pic S9(9)V99.
              07 EINH-EMP               pic S9(10)V9.
              07 ST-NR                  pic 9(7).
              07 DIFF-PROV-STF          pic S999V99.
              07 VM-GESCHFT-KZ          pic X.
              07 EIGEN-KZ               pic X.
              07 PPFL-SUMME             pic S9(9)V99.
              07 FAELLIG-VON            pic X(10).
              07 PDPT-ID                pic 9(15).
              07 VNR                    pic X(15).
              07 RFOLGE-NR              pic 999.
              07 HAFTZEIT-KZ            pic XX.
              07 FIRST-INSERT           pic X(26).
              07 MA-ABR-DAT             pic X(10).
              07 DYN-KZ-1               pic 9(4).
              07 DYN-KZ-2               pic 9(4).
              07 POL-DAT                pic X(10).
              07 PRO-RATA-FAK-Z         pic 999.
              07 PRO-RATA-FAK-N         pic 999.
              07 PROD-PROD-SCHL         pic 999.
              07 PROV-SCHL              pic X(5).
              07 EMP-TYP                pic 9(4).
              07 RAB-STF                pic X(4).
              07 RWERT-SUMME            pic S9(9)V99.
              07 SAMMEL-NR              pic 9(9).
              07 BESVV                  pic 9.
              07 PGV                    pic 9(4).
              07 BUZ-TARIF              pic 9(4).
              07 BEWSCHL                pic X(7).
              07 UST-BETRAG-AUS         pic S9(9)V99.
              07 UST-BETRAG-EINB        pic S9(9)V99.
              07 UST-KORREKTUR-KZ       pic X.
              07 KORREKTUR-ZIEL-STATUS  pic X.
              07 UST-BERECH-KZ          pic X.
              07 UST-ABR-NR             pic X(15).
              07 UST-SATZ               pic 99V99.
              07 PG-RECH-NR             pic X(12).
              07 FAELLIG-BIS            pic X(10).
              07 PMPT-ID                pic 9(15).
              07 STEUERJAHR             pic X(4).
              07 VSP-FLEXN-RELEVANZ     pic X.
              07 VSP-FLEXN-FOLGE-KZ     pic X.
              07 VSP-PRODUKT-KOMBI-NR   pic 9(5).
              07 VSP-PRODUKT-KOMBI-BEZ  pic XXX.
              07 VSP-FLEXN-BERECHTIGUNG pic X.
      *
      **************************************************************

