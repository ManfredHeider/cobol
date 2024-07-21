      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5092.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5092.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:28:54   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 28 2015 15:43:32   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.0   May 27 2015 14:26:06   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Referenztab. Prod. Gesamt
      *
      *     SOURCE-NAME      :  DRMA5092
      *
      *     GENERIERUNG      :  02.10.2017/13:54:40
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!26.05.15!2.1  ! Neuerstellung             ! D.WILHELM
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DEMA0128         ! S.WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5092.
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 PG                     pic S9(4)      binary.
              07 PRODSCHL               pic S9(4)      binary.
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
      *
      **************************************************************

