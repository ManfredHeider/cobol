      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.12.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5126.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5126.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:30:22   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Mar 16 2017 13:15:10   t13ex15
      *AMA00297: Erweiterung der SST FlexN - (SUH) Copys
      *
      *   Rev 2.2   Mar 21 2016 19:23:22   t13ac19
      *AMA00278: Erweiterung der DAISY-SST um das Datenfeld Steuerj
      *ahr - Copies
      *
      *   Rev 2.1   Jan 19 2016 12:04:02   t13ac14
      *AMA00218: USt: LUCY I - Nacharbeiten aus AMA00200
      *
      *   Rev 2.0   Dec 09 2015 14:19:40   t13ac14
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     DATENSATZ: DAISY-BEWEGUNGS-BASIS-DATENSÄTZE
      *
      *     SOURCE-NAME      :  DRMA5126
      *
      *     GENERIERUNG      :  06.10.2017/09:28:54
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------+--------+-----+---------------------------+----------
      * AMA00218!09.12.15!2.1  ! NEUERSTELLUNG             !VON HELDEN
      * AMA00278!04.03.16!2.2  ! STEUERJAHR EINGEFÜGT;     ! O.WILHELM
      *         !        !     ! DEMA0429: AUSPRÄGUNG VON  !
      *         !        !     !        06 IN 04 GEÄNDERT  !
      * AMA00297!21.02.17!2.3  ! FLEXN FELDER EINGEFÜGT    ! S.WARZ
      * AMA00360!20.09.17!2.4  ! ÄNDERUNG DEMA0128         ! S.WARZ
      *         !        !     ! ÄNDERUNG DGMA0111         !
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5126.
              07 SATZ-KZ                pic X.
              07 PGEB-GES               pic S9(4)      binary.
              07 PROD-PROD-SCHL-SORT    pic S9(4)      binary.
              07 VNR-SORT               pic X(15).
              07 TARIF-KZ               pic S9(4)      binary.
              07 TARIF-BEZEICHNUNG      pic X(15).
              07 KUNDENNAME             pic X(20).
              07 VP-NAME                pic X(20).
              07 DGV                    pic S9(4)      binary.
              07 AVERM                  pic 9(7)       binary.
              07 PRODART                pic 9(4)       binary.
              07 HAFTZ-VON              pic X(10).
              07 HAFTZ-BIS              pic X(10).
              07 PRO-RATA-SUMME         pic S9(9)V99   packed-decimal.
              07 PRO-RATA-SUMME-GES     pic S9(9)V99   packed-decimal.
              07 VM-GESCHFT-KZ          pic X.
              07 PPFL-SUMME             pic S9(9)V99   packed-decimal.
              07 FAELLIG-VON            pic X(10).
              07 PDPT-ID                pic S9(15)     packed-decimal.
              07 VNR                    pic X(15).
              07 RFOLGE-NR              pic S9(4)      binary.
              07 HAFTZEIT-KZ            pic XX.
              07 FIRST-INSERT           pic X(26).
              07 MA-ABR-DAT             pic X(10).
              07 DYN-KZ-1               pic S9(4)      binary.
              07 DYN-KZ-2               pic S9(4)      binary.
              07 POL-DAT                pic X(10).
              07 PRO-RATA-FAK-Z         pic S9(4)      binary.
              07 PRO-RATA-FAK-N         pic S9(4)      binary.
              07 PROD-PROD-SCHL         pic S9(4)      binary.
              07 PROV-SCHL              pic X(5).
              07 RAB-STF                pic S9(4)      binary.
              07 RWERT-SUMME            pic S9(9)V99   packed-decimal.
              07 SAMMEL-NR              pic S9(9)      binary.
              07 BESVV                  pic 9.
              07 PGV                    pic S9(4)      binary.
              07 BUZ-TARIF              pic S9(4)      binary.
              07 VERTRAGSDAUER          pic 9(04).
              07 ZAHLWEISE              pic S9(4)      binary.
              07 INKASSOSTAND           pic X(10).
              07 VN-STRASSE             pic X(30).
              07 VN-PLZ                 pic X(5).
              07 VN-ORT                 pic X(30).
              07 PG-PRODART             pic S9(4)      binary.
              07 EINH-BEW               pic S9(6)V9    packed-decimal.
              07 BEWSCHL                pic X(7).
              07 UST-KORREKTUR-KZ       pic X.
              07 UST-SATZ               pic S999V99    packed-decimal.
              07 PG-RECH-NR             pic X(12).
              07 FAELLIG-BIS            pic X(10).
              07 PMPT-ID                pic S9(15)     packed-decimal.
              07 STEUERJAHR             pic X(4).
              07 VSP-FLEXN-RELEVANZ     pic X.
              07 VSP-FLEXN-FOLGE-KZ     pic X.
              07 VSP-PRODUKT-KOMBI-NR   pic S9(5)      binary.
              07 VSP-PRODUKT-KOMBI-BEZ  pic XXX.
              07 VSP-FLEXN-BERECHTIGUNG pic X.
              07 ANZAHL-PROV-EMP        pic S9(4)      binary.
              07 SATZLAENGE             pic S9(4)      binary.
              07 STRUKTUR               occurs 20.
                 09 PROV-BETRAG-AUS     pic S9(9)V99   packed-decimal.
                 09 UST-BETRAG-AUS      pic S9(9)V99   packed-decimal.
                 09 PROV-BETRAG-EINB    pic S9(9)V99   packed-decimal.
                 09 UST-BETRAG-EINB     pic S9(9)V99   packed-decimal.
                 09 BUCH-RELEVANT       pic X.
                 09 EMP-TYP             pic S9(4)      binary.
                 09 DIFF-PROV-STF       pic S9(4)      binary.
                 09 ST-NR               pic 9(7)       binary.
                 09 EINH-EMP            pic S9(6)V9    packed-decimal.
                 09 EIGEN-KZ            pic X.
                 09 UST-BERECH-KZ       pic X.
                 09 UST-ABR-NR          pic X(15).
                 09 KORREKTUR-ZIEL-STATUS
                                        pic X.
      *
      **************************************************************

