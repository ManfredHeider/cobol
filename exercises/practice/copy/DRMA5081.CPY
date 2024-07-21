      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  19.03.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5081.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRMA5081.CPv  $
      *
      *   Rev 2.4   Oct 19 2017 14:28:10   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.3   Jul 28 2015 15:42:46   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.2   Mar 20 2015 10:44:50   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.1   Mar 19 2015 17:25:04   t13ac20
      *AMA00148: USt: KOMA0505-VB-Abrechnung Druck - allgemeine Pro
      *gramme
      *
      *   Rev 2.0   Mar 19 2015 10:48:42   t13ac20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     MITCOPY: REFERENZTABELLE DER PRODUKTE
      *
      *     SOURCE-NAME      :  DRMA5081
      *
      *     GENERIERUNG      :  02.10.2017/13:51:31
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00148!19.03.15!   1 ! NEUERSTELLUNG             ! WOLFF
      * AMA00148!20.03.15!   2 ! DATENELEMENTSNAMEN ÜBER-  ! WOLFF
      *         !        !     ! ARBEIET (ANGEPASST AN     !
      *         !        !     ! DDMA0513)                 !
      * AMA00128!08.06.15!   3 ! DIV. ELEMENTE ERGÄNZT     ! D.WILHELM
      * AMA00360!20.09.17!   4 ! ÄNDERUNG DEMA0128         ! S.WARZ
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5081.
              07 VERSION                pic X.
              07 VG                     pic 9(4).
              07 PG                     pic 99.
              07 PRODSCHL               pic 999.
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 PROD-BEZEICH-KURZ      pic X(10).
              07 PROD-BEZEICH-LANG      pic X(40).
              07 PRODSPARTE             pic S9(4)      binary.
              07 KNR-ART-PROV-TYP       pic S9(4)      binary.
              07 IND-JW                 pic S9(4)      binary.
              07 PRODART-MIN            pic XX.
              07 PRODART-MAX            pic XX.
              07 TARIFBEZ               pic X(8).
              07 UST-SATZ               pic S999V99    packed-decimal.
              07 SL-KZN                 pic S9(4)      binary.
              07 ZUSATZTAR-KZN          pic X.
              07 ZUSATZTAR-BEZEICH      pic X(16).
              07 ERWERB-FOLGE-KZN       pic X.
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************

