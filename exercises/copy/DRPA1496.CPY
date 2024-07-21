      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRPA1496.CPY  $
      *    $Revision:   2.5  $
      *
      *    $Log:   L:/mf/CPY/DRPA1496.CPv  $
      *
      *   Rev 2.5   Aug 26 2019 12:04:04   t13ex67
      *APA02843: WEPA0008
      *
      *   Rev 2.4   Jul 16 2015 15:10:56   t13ae37
      *APA01932: ABU / Schnittstellen und Referenz
      *
      *   Rev 2.3   Oct 08 2014 15:18:34   t13ex67
      *APA01702: Erweiter. WEPA0008 um Historien-Tab. (TBPA0313/315
      */317/322)
      *
      *   Rev 2.2   16 May 2007 06:46:18   t13ex44
      *APA00604: Anpassungen in Daisy zur Ablösung PEW
      *
      *   Rev 2.1   17 Oct 2001 16:30:20   t13ex21
      *FPA00262 - Neuerstellung
      *
      *   Rev 2.0   10 Apr 2001 08:42:24   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRPA1496  RESTART PMPA0040                   *
      *    -------------------------------------------------------   *
      *    ERSTELLT: STORCH                                          *
      *    DATUM:    19.06.2001                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * FPA00???!19.06.01! 01 ! NEUERSTELLUNG             ! STORCH   *
      * APA00604!12.04.07!2.2 ! ABSCHALTUNG PEW           ! SCHEIDEL *
      * APA01702!13.12.13!2.3 ! ERWEITERUNG UM HISTOR.TAB.! STRAUSS  *
      *         !        !    ! PMBEH,PMPRH,PMPTH,PDPTH   !          *
      * APA01932!30.06.15!2.4 ! EINFÜHRUNG UMSATZSTEUER   ! BUCH     *
      * APA02843!19.08.19!2.5 ! ERWEITERUNG UM PDPE       ! STORCH   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
          01 RST-PMPA0040.
           05 RST-COPYNAME            PIC  X(008)  VALUE 'DRPA1496'.
           05 RST-PMPA0040-INFOS.
            10 RST-GES                PIC  9(004).
            10 RST-VNR                PIC  9(015).
            10 RST-ANZ-PVER           PIC S9(009) BINARY.
            10 RST-ANZ-PVPT           PIC S9(009) BINARY.
            10 RST-ANZ-PINZ           PIC S9(009) BINARY.
            10 RST-ANZ-PVOR           PIC S9(009) BINARY.
            10 RST-ANZ-PNAP           PIC S9(009) BINARY.
            10 RST-ANZ-PAEV           PIC S9(009) BINARY.
            10 RST-ANZ-PMBE           PIC S9(009) BINARY.
131213      10 RST-ANZ-PMBEH          PIC S9(009) BINARY.
            10 RST-ANZ-PMPR           PIC S9(009) BINARY.
131213      10 RST-ANZ-PMPRH          PIC S9(009) BINARY.
            10 RST-ANZ-PMPT           PIC S9(009) BINARY.
131213      10 RST-ANZ-PMPTH          PIC S9(009) BINARY.
            10 RST-ANZ-PDPT           PIC S9(009) BINARY.
131213      10 RST-ANZ-PDPTH          PIC S9(009) BINARY.
            10 RST-ANZ-PEST           PIC S9(009) BINARY.
            10 RST-ANZ-PMPE           PIC S9(009) BINARY.
            10 RST-ANZ-PMPEH          PIC S9(009) BINARY.
            10 RST-ANZ-PMPG           PIC S9(009) BINARY.
            10 RST-ANZ-PMPGH          PIC S9(009) BINARY.
            10 RST-ANZ-PMPP           PIC S9(009) BINARY.
            10 RST-ANZ-PMPPH          PIC S9(009) BINARY.
            10 RST-ANZ-PMDS           PIC S9(009) BINARY.
            10 RST-ANZ-PDTA           PIC S9(009) BINARY.
            10 RST-ANZ-PAMS           PIC S9(009) BINARY.
            10 RST-ANZ-PVKB           PIC S9(009) BINARY.
            10 RST-ANZ-ANTRAG         PIC S9(009) BINARY.
            10 RST-ANZ-STRUK          PIC S9(009) BINARY.
290615      10 RST-ANZ-PABR           PIC S9(009) BINARY.
190819      10 RST-ANZ-PDPE           PIC S9(009) BINARY.
190819      10 RST-ANZ-PDPEH          PIC S9(009) BINARY.

