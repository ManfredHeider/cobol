      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3049.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3049.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:15:46   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jul 01 2013 11:39:20   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVPROD_EINB_REF00
      *
      *     SOURCE-NAME      :  DRMA3049
      *
      *     GENERIERUNG      :  02.10.2017/12:55:38
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 PROD-EINB-REF.
              07 PG                     pic S9(4)      binary.
              07 PRODSCHL               pic S9(4)      binary.
              07 SEL-WERT               pic X(15).
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 PROD-KURZ              pic X(10).
              07 PROD-LANG              pic X(40).
              07 HZ-TYP                 pic X(08).
              07 HZ-PG                  pic S9(4)      binary.
              07 HZ-VB                  pic S9(4)      binary.
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
      *
      **************************************************************

