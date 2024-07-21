      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1240.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA1240.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:13:02   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jun 28 2013 16:20:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Eingabe-Bereich UPMA1235 - UPDATE
      *
      *     SOURCE-NAME      :  DRMA1240
      *
      *     GENERIERUNG      :  05.10.2017/08:53:58
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00360!20.09.17!2    ! ÄNDERUNG DRMA1106 EINBEH. ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA1240.
              07 FUNKTION               pic X(8).
              07 EINBEHALT.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 EINB-SATZ           pic S999V99    packed-decimal.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
      *
      **************************************************************

