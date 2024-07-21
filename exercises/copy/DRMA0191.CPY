      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0191.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA0191.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:10:28   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Jul 28 2015 15:42:00   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.1   Jul 01 2013 17:07:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      **************************************************************
      *
      *     Datensatz: OVPRODUKT00
      *
      *     SOURCE-NAME      :  DRMA0191
      *
      *     GENERIERUNG      :  02.10.2017/12:29:51
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!2.1  ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00128!01.06.15!2.2  ! Änderung                  ! D.WILHELM
      *         !        !     ! IND-JW statt DVAG-IND-JW  !
      *         !        !     !          und AVBG-IND-JW  !
      *         !        !     ! neue Attribute:           !
      *         !        !     ! VERSION                   !
      *         !        !     ! VG,                       !
      *         !        !     ! GUELTIG-AB                !
      *         !        !     ! GUELTIG-BIS               !
      *         !        !     ! TARIFBEZ                  !
      *         !        !     ! UST-SATZ                  !
      *         !        !     ! SL-KZN                    !
      *         !        !     ! ZUSATZTAR-KZN             !
      *         !        !     ! ZUSATZTAR-BEZEICH         !
      *         !        !     ! ERWERB-FOLGE-KZN          !
      * AMA00360!20.09.17!2.3  ! ÄNDERUNG DRMA1493         ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0191.
              07 PRODUKT.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 PG                  pic S9(4)      binary.
                 09 PRODSCHL            pic S9(4)      binary.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 PROD-KURZ           pic X(10).
                 09 PROD-LANG           pic X(40).
                 09 PRODSPARTE          pic S9(4)      binary.
                 09 KNR-ART-PROV-TYP    pic S9(4)      binary.
                 09 IND-JW              pic S9(4)      binary.
                 09 PRODART-MIN         pic XX.
                 09 PRODART-MAX         pic XX.
                 09 TARIFBEZ            pic X(8).
                 09 UST-SATZ            pic S999V99    packed-decimal.
                 09 SL-KZN              pic S9(4)      binary.
                 09 ZUSATZTAR-KZN       pic X.
                 09 ZUSATZTAR-BEZEICH   pic X(16).
                 09 ERWERB-FOLGE-KZN    pic X.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************

