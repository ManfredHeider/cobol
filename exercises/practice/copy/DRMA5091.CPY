      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.05.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5091.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5091.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:28:44   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 28 2015 15:43:18   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.0   May 27 2015 14:25:42   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Arbeitssatz Referenztab. Prod. manueller Teil
      *
      *     SOURCE-NAME      :  DRMA5091
      *
      *     GENERIERUNG      :  02.10.2017/13:54:00
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
           05 DRMA5091.
              07 PG                     pic S9(4)      binary.
              07 PRODSCHL               pic S9(4)      binary.
              07 DVAG-IND-JW            pic 99.
              07 AVBG-IND-JW            pic 99.
              07 ALLF-IND-JW            pic 99.
              07 PRODART-MIN            pic XX.
              07 PRODART-MAX            pic XX.
              07 TARIFBEZ               pic X(8).
              07 ZUSATZTAR-KZN          pic X.
              07 ZUSATZTAR-BEZEICH      pic X(16).
              07 ERWERB-FOLGE-KZN       pic X.
      *
      **************************************************************

