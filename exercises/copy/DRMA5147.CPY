      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.05.2016
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2016 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA5147.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRMA5147.CPv  $
      *
      *   Rev 2.1   Jul 07 2016 16:04:00   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.0   May 10 2016 11:25:36   t13ac18
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich ZTMA0212 - INSERT
      *
      *     SOURCE-NAME      :  DRMA5147
      *
      *     GENERIERUNG      :  12.05.2016/14:41:58
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00292!10.05.16!2.1  ! Neuerstellung             ! D.WILHELM
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA5147.
              07 FUNKTION               pic X(8).
              07 PRODUKTGEBER.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 PG                  pic S9(4)      binary.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 PG-KURZ             pic X(10).
                 09 PG-LANG             pic X(40).
                 09 LAND-KZN            pic X(4).
                 09 BEW-ART-KZN         pic X(10).
                 09 KTO                 pic X(10).
                 09 PG-UST-RELEVANT     pic X.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************

