      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  22.11.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5159.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA5159.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:31:38   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Dec 21 2016 15:24:34   t13ac18
      *AMA00296: USt: LUCY II - CR Erweiterung Deckblatt - Sourcen
      *
      *   Rev 2.0   Nov 22 2016 14:17:26   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     ARBEITSBESTAND FÜR UST.-KORREKTUREN
      *
      *     SOURCE-NAME      :  DRMA5159
      *
      *     GENERIERUNG      :  02.10.2017/14:08:28
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00296!22.11.16!2.1  ! NEUERSTELLUNG             ! D.WILHELM
      * AMA00360!20.09.17!2.2  ! ÄNDERUNG DEMA0128         ! S.WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 ARBEITSBESTAND.
              07 VBNR                   pic 9(9).
              07 PROV-AUS-KU            pic S9(9)V99   packed-decimal.
              07 PROV-AUS-U             pic S9(9)V99   packed-decimal.
              07 PROV-UST               pic S9(9)V99   packed-decimal.
              07 UST-SATZ               pic S999V99    packed-decimal.
              07 ABR-NR-KORR            pic X(15).
              07 ABR-NR-AKT             pic X(15).
              07 PG-RECH-NR             pic X(12).
              07 KORREKTUR-ZIEL-STATUS  pic X.
              07 STEUERJAHR             pic X(4).
              07 VERTRAGS-ID            pic X(15).
              07 PRODSCHL               pic 999.
              07 PRODART                pic 99.
              07 BUCHTEXT               pic X(92).
              07 DV-KENNUNG             pic X(10).
              07 REFERENZ-NR            pic X(16).
              07 FIBU-SORT-ID           pic 9(4).
              07 FIBU-SORT-LFDNR        pic 9(4).
              07 BEW-ERSTELL-ZPKT       pic X(26).
              07 KNR-VERARB-TYP         pic S9(4)      binary.
              07 BUCHDAT                pic X(10).
      *
      **************************************************************

