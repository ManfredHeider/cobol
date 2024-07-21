      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5095.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5095.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:29:14   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 28 2015 15:44:18   t13ac18
      *AMA00128: USt: KOMA0504-Übernahme - Teil 1 - Copies Übernahm
      *e Referenztabelle
      *
      *   Rev 2.0   Jun 02 2015 15:17:58   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich ZTMA0211 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA5095
      *
      *     GENERIERUNG      :  02.10.2017/13:56:29
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00128!02.06.15!2.1  ! Neuerstellung             ! D.WILHELM
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DRMA1493         ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5095.
              07 FUNKTION               pic X(8).
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

