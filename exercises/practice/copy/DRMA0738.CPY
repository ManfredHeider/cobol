      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0738.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0738.CPv  $
      *
      *   Rev 2.2   Apr 12 2016 11:24:30   t13ac20
      *AMA00268: USt: LUCY II - KOMA0508-Zugriffe - Copies
      *
      *   Rev 2.1   Jun 28 2013 16:02:40   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     Datensatz: OVSTAMMDATEN00
      *
      *     SOURCE-NAME      :  DRMA0738
      *
      *     GENERIERUNG      :  18.03.2016/13:42:45
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      * AMA00268!11.03.16!2.2  ! DRMA3316 GEÄNDERT         ! ARRUDA
      *         !        !     !                           !
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
      *
      *
           05 DRMA0738.
              07 STAMMDATEN.
                 09 VERSION             pic X.
                 09 VG                  pic S9(4)      binary.
                 09 VBNR                pic S9(9)      binary.
                 09 HIST-KZN            pic X.
                 09 GUELTIG-AB          pic X(10).
                 09 GUELTIG-BIS         pic X(10).
                 09 KTO-FKT             pic XX.
                 09 EROEFF-DAT          pic X(10).
                 09 SCHLIE-DAT          pic X(10).
                 09 EINR-DAT            pic X(10).
                 09 GEB-DAT             pic X(10).
                 09 DIR-NR              pic S9(4)      binary.
                 09 STRUK-STUFE         pic S9(4)      binary.
                 09 STRUK-STUFE-TEXT    pic X(4).
                 09 STELLEN-TYP         pic S9(4)      binary.
                 09 PRAXIS-STUFE        pic S9(4)      binary.
                 09 UMST-NR             pic X(15).
                 09 REGISTER-NR         pic X(30).
                 09 AM-AGENT-NR         pic S9(9)      binary.
                 09 VERRECHNKTO-KZ      pic X.
                 09 MA-ALLFI-STATUS     pic X.
                 09 MA-KEINE-ERF-PROV   pic X.
                 09 AVERM-KZN           pic X.
                 09 MA-CKV-WECHSEL      pic X.
                 09 MA-CKV-ERFPROV      pic X.
                 09 SCHLIE-DAT-ORIG     pic X(10).
                 09 STRUK-STUFE-ORIG    pic S9(4)      binary.
                 09 MA-SCHW-PARTNER     pic X.
                 09 AENDERUNGSSYSZEIT   pic X(26).
                 09 AENDERUNGSKENNUNG   pic X(8).
                 09 KNR-USTAT-TYP       pic S9(4)      binary.
      *
      **************************************************************

