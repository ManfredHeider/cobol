      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3469.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRMA3469.CPv  $
      *
      *   Rev 2.5   Oct 19 2017 14:19:08   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.4   May 18 2016 11:19:40   t13ac20
      *AMA00263: USt: LUCY II - KOMA0505-VB-Abrechnung - Copies
      *
      *   Rev 2.3   Jul 28 2015 14:49:34   t13ac19
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies
      *
      *   Rev 2.2   Oct 15 2013 09:46:04   t13ac17
      *AMA00018: Anpassung Daisy - SST-Übernahme Buchen
      *
      *   Rev 2.1   Jul 01 2013 17:14:08   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Einzelnachweis - var. Teil (FIBU)
      *
      *     SOURCE-NAME      :  DRMA3469
      *
      *     GENERIERUNG      :  02.10.2017/13:18:45
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00011!24.09.13!2.1  ! ÜBERNAHME AUS MIGRATION   ! WALBERT
      * AMA00018!01.10.13!2.2  ! DAISY ANPASSUNG           ! WALBERT
      * AMA00129!24.06.15!2.3  ! Erweiterung um BUCHTEXT   ! O.Wilhelm
      * AMA00263!18.04.16!2.4  ! Einfügen von              ! KUBE
      *         !        !     ! DGMA5007, DGMA5008,       !
      *         !        !     ! DEMA0476, DEMA0477,       !
      *         !        !     ! DEMA0478, DEMA0479,       !
      *         !        !     ! DEMA0480 und Entfernen von!
      *         !        !     ! KZN-SONSTBUCH             !
      * AMA00360!20.09.17!2.5  ! ÄNDERUNG DGMA5007 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 FIBU-DATEN.
              07 BUCHTEXT               pic X(92).
              07 FIBU-SORT-ID           pic 9(4).
              07 FIBU-SORT-LFDNR        pic 9(4).
              07 UST-PFLICHTIG-KZN      pic X.
              07 DGMA5007.
                 09 UST-BERECH-KZN      pic X.
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 PROV-AUS-EINB-UST   pic S9(9)V99   packed-decimal.
                 09 PROV-AUS-EINB       pic S9(9)V99   packed-decimal.
                 09 PROV-AUS            pic S9(9)V99   packed-decimal.
                 09 PROV-EINB           pic S9(9)V99   packed-decimal.
                 09 PROV-UST            pic S9(9)V99   packed-decimal.
              07 DGMA5008.
                 09 UST-KORREKTUR-KZN   pic X.
                 09 KORREKTUR-ZIEL-STATUS
                                        pic X.
                 09 ABR-NR-KORR         pic X(15).
                 09 STEUERJAHR          pic X(4).
              07 REFERENZ-NR            pic X(16).
              07 KNR-VERARB-TYP         pic S9(4)      binary.
      *
      **************************************************************

