      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum     :  26.04.2013
      *            Kategorie :  Cobol-Programm
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRMA0587.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRMA0587.CPv  $
      *
      *   Rev 2.2   Oct 19 2017 14:10:38   t13ex15
      *AMA00360: 3. Statistik-Kategorie Folge Neu Copys
      *
      *   Rev 2.1   Jun 28 2013 16:01:28   p13sp20
      *AMA00011: Kalypso: Teilprojekt 4
      *
      *   Rev 2.0   Apr 26 2013 12:00:00   p13sp20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      **************************************************************
      *
      *     SST VB-Kontenstände Ultimo DVAG
      *
      *     SOURCE-NAME      :  DRMA0587
      *
      *     GENERIERUNG      :  02.10.2017/12:33:32
      *     DURCH            :  PMMA5004/01.00.01
      *
      * LOGBUCH
      * AENDERUNGEN:
      * AUFTRAG ! DATUM  !VER  ! AENDERUNG                 ! NAME
      * --------------------------------------------------------------
      * AMA00153!10.04.15!1    ! MASCHINELLE ERSTELLUNG    ! D.WILHELM
      *         !        !     ! KONNTE SCHON IN AC NICHT  !
      *         !        !     ! GENERIERT WERDEN, KOPIE   !
      *         !        !     ! VON DRRW0006              !
      * AMA00360!20.09.17!2    ! ÄNDERUNG EINBEHALT        ! WARZ
      *         !        !     !                           !
      *
      * ENDELOGBUCH
      **************************************************************
           02 AFC-HV-KTO-SATZ.
      *    * Feld 1: VB-Nummer (7st. + Prüfziffer)
              05 HVNUMMER                PIC X(08).
              05 TZ-01                   PIC X(01).
              05 BUCHPERIODE             PIC X(06).
              05 TZ-02                   PIC X(01).
              05 DIREKTION               PIC X(08).
              05 TZ-03                   PIC X(01).
              05 WAEHRUNGSKZ             PIC X(03).
              05 TZ-04                   PIC X(01).
              05 BETRDISKONT             PIC 9(18).
              05 TZ-05                   PIC X(01).
              05 SOLLDISKONT             PIC 9(01).
              05 TZ-06                   PIC X(01).
              05 BETREINB                PIC 9(18).
              05 TZ-07                   PIC X(01).
              05 SOLLEINB                PIC 9(01).
              05 TZ-08                   PIC X(01).
              05 BETRVORSCH              PIC 9(18).
              05 TZ-09                   PIC X(01).
              05 SOLLVORSCH              PIC 9(01).
              05 TZ-10                   PIC X(01).
              05 BETRABSCHR              PIC 9(18).
              05 TZ-11                   PIC X(01).
              05 SOLLABSCHR              PIC 9(01).
              05 TZ-12                   PIC X(01).
              05 BETRANWALT              PIC 9(18).
              05 TZ-13                   PIC X(01).
              05 SOLLANWALT              PIC 9(01).
              05 TZ-14                   PIC X(01).
              05 BETRGESAMT              PIC 9(18).
              05 TZ-15                   PIC X(01).
              05 SOLLGESAMT              PIC 9(01).
              05 TZ-16                   PIC X(01).
              05 JAHRBASISSOLLSALDO      PIC 9(04).
              05 TZ-17                   PIC X(01).
              05 JAHRERSTABSCHR          PIC 9(04).
              05 TZ-18                   PIC X(01).
              05 AENDERUNGSKZ            PIC 9(01).
              05 TZ-19                   PIC X(01).
              05 FEHLERKZ                PIC 9(01).
              05 TZ-20                   PIC X(01).
              05 STORNOKZ                PIC 9(01).
              05 TZ-21                   PIC X(01).
              05 FUNKTIONSKZ             PIC X(03).
              05 TZ-22                   PIC X(01).
              05 DOPPELKZ                PIC 9(01).

