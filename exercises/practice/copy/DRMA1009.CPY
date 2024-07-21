      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA1009.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRMA1009.CPv  $
      *
      *   Rev 2.3   Oct 19 2017 14:12:20   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.2   Oct 14 2013 10:12:00   t13ac20
      *AMA00020: Rückstellungsabrechnung altes und neues Verfahren:
      * Copies
      *
      *   Rev 2.1   Jun 28 2013 16:11:26   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datenbereich zu Baustein DPMA3507
      *
      *     SOURCE-NAME      :  DRMA1009
      *
      *     GENERIERUNG      :  02.10.2017/12:43:13
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      *         !        !     ! KONNTE SCHON IN AC NICHT  !
      *         !        !     ! GENERIERT WERDEN          !
      * AMA00360!20.09.17!2    ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
       01  ZW-EINBEHALTSDATUM.
      *    EINB-DATUM im Format TT.MM.JJJJ
           05 ZW-EINB-DATUM-A.
              10 ZW-EINB-TT-A     pic 99      value zero.
              10                   pic X       value '.'.
              10 ZW-EINB-MM-A     pic 99      value zero.
              10                   pic X       value '.'.
              10 ZW-EINB-JJJJ-A   pic 9(4)    value zero.
      *    EINB-DATUM im Format JJJJMMTT
           05 ZW-EINB-DATUM-B     pic 9(8)    value zero.
           05            redefines ZW-EINB-DATUM-B.
              10 ZW-EINB-JJJJ-B   pic 9(4).
              10 ZW-EINB-MM-B     pic 99.
              10 ZW-EINB-TT-B     pic 99.
      *    EINB-DATUM im Format JJJJ-MM-TT
           05 ZW-EINB-DATUM-ISO.
              10 ZW-EINB-JJJJ-I   pic 9(4)    value zero.
              10                   pic X       value '-'.
              10 ZW-EINB-MM-I     pic 99      value zero.
              10                   pic X       value '-'.
              10 ZW-EINB-TT-I     pic 99      value zero.
      *
      **************************************************************

