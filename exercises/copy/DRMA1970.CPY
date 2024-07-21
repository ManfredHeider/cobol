      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1970.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1970.CPv  $
      *
      *   Rev 2.2   Jul 07 2016 16:03:00   t13ac18
      *AMA00292: Umstellung OVGESELLSCHAFT00 - Sourcen
      *
      *   Rev 2.1   Jul 01 2013 10:38:54   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVPRODUKTGEBER00
      *
      *     SOURCE-NAME      :  DRMA1970
      *
      *     GENERIERUNG      :  12.05.2016/15:37:34
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00292!11.05.16!2.2  ! UMBEN. OVGESELLSCHAFT00   ! D.WILHELM
      *         !        !     ! IN OVPRODUKTGEBER00       !
      *         !        !     ! ÄNDERUNG DRMA1485         !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1970.
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

