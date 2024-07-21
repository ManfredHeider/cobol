      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA3316.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA3316.CPv  $
      *
      *   Rev 2.2   Apr 12 2016 11:24:42   t13ac20
      *AMA00268: USt: LUCY II - KOMA0508-Zugriffe - Copies
      *
      *   Rev 2.1   Jul 01 2013 11:46:02   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     mitCopy: Tabelle OVSTAMMDATEN00
      *
      *     SOURCE-NAME      :  DRMA3316
      *
      *     GENERIERUNG      :  18.03.2016/13:49:44
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00153!02.03.16!OHNE ! DEAT0001 ERSETZT BEI      ! D.WILHELM
      *         !        !     ! VERRECHNKTO-KZ            !
      *         !        !     ! MA-ALLFI-STATUS           !
      *         !        !     ! MA-KEINE-ERF-PROV         !
      *         !        !     ! (OHNE AUSW. AUF GEN.)     !
      * AMA00268!11.03.16!2.2  ! KNR-USTAT-TYP ERGÄNZT     ! ARRUDA
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 STAMMDATEN.
              07 VERSION                pic X.
              07 VG                     pic S9(4)      binary.
              07 VBNR                   pic S9(9)      binary.
              07 HIST-KZN               pic X.
              07 GUELTIG-AB             pic X(10).
              07 GUELTIG-BIS            pic X(10).
              07 KTO-FKT                pic XX.
              07 EROEFF-DAT             pic X(10).
              07 SCHLIE-DAT             pic X(10).
              07 EINR-DAT               pic X(10).
              07 GEB-DAT                pic X(10).
              07 DIR-NR                 pic S9(4)      binary.
              07 STRUK-STUFE            pic S9(4)      binary.
              07 STRUK-STUFE-TEXT       pic X(4).
              07 STELLEN-TYP            pic S9(4)      binary.
              07 PRAXIS-STUFE           pic S9(4)      binary.
              07 UMST-NR                pic X(15).
              07 REGISTER-NR            pic X(30).
              07 AM-AGENT-NR            pic S9(9)      binary.
              07 VERRECHNKTO-KZ         pic X.
              07 MA-ALLFI-STATUS        pic X.
              07 MA-KEINE-ERF-PROV      pic X.
              07 AVERM-KZN              pic X.
              07 MA-CKV-WECHSEL         pic X.
              07 MA-CKV-ERFPROV         pic X.
              07 SCHLIE-DAT-ORIG        pic X(10).
              07 STRUK-STUFE-ORIG       pic S9(4)      binary.
              07 MA-SCHW-PARTNER        pic X.
              07 AENDERUNGSSYSZEIT      pic X(26).
              07 AENDERUNGSKENNUNG      pic X(8).
              07 KNR-USTAT-TYP          pic S9(4)      binary.
      *
      **************************************************************

