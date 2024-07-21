      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0012.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVD0012.CPv  $
      *
      *   Rev 2.4   Jun 03 2015 12:36:54   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - Tabelle MS erweit
      *ern
      *
      *   Rev 2.3   16 Aug 2007 17:48:06   t13ex22
      *AVD00044: VBI Release 4.10
      *
      *   Rev 2.2   Dec 07 2004 16:37:20   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.1   Sep 20 2004 10:04:24   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 10 2004 09:19:26   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVD0012  VBI-SCHNITTSTELLE STVD0030          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.08.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVD00001!10.08.04!2.1 ! NEU                       ! KLEEMEIER *
061204* AVD00001!06.12.04!2.2 ! MS-PROV-STF-AUSF HINZU    ! KLEEMEIER *
110707* AVD00044!07.07.07!2.3 ! 4 NEUE FELDER AUS C-VIEW  ! MUND      *
150515* AVB01332!15.05.15!2.4 ! 3 NEUE FELDER AUS D-VIEW  ! Walb      *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DER VBI-SCHNITTSTELLE       *
      *    STVD0030 - MITARBEITERSTUFEN (MS).                         *
      *                                                               *
      *****************************************************************
      *
       01  STVD0030-SATZ.
           10 MS-NR-K              PIC S9(9) USAGE COMP.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 MS-EINSTIEG          PIC X(1).
           10 MS-HPT               PIC X(1).
           10 MS-34C               PIC X(1).
           10 MS-ABZPROZ-REGEL     PIC S999V99 USAGE COMP-3.
           10 MS-EING-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-EIG-GES        PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-EIG-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-GRU-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-EH-GES-PZTR       PIC S999999999V9 USAGE COMP-3.
           10 MS-WEIT-EH-EIG       PIC S999999999V9 USAGE COMP-3.
           10 MS-ZEITRAUM          PIC S9(4) USAGE COMP.
           10 MS-EQF-EIG           PIC S999V99 USAGE COMP-3.
           10 MS-EQF-GRU           PIC S999V99 USAGE COMP-3.
           10 MS-EQF-GES           PIC S999V99 USAGE COMP-3.
           10 MS-PROV-STF          PIC S999V99 USAGE COMP-3.
           10 MS-GUELTIG-BIS       PIC X(10).
           10 MS-BEZ-KURZ          PIC X(4).
           10 MS-BEZ               PIC X(40).
           10 MS-BEZ-KURZ-AUSF     PIC X(4).
           10 MS-BEZ-AUSF          PIC X(40).
           10 MS-FG                PIC X(1).
           10 MS-127               PIC X(1).
           10 KTY-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 KNR-GESELL-TYP       PIC S9(4) USAGE COMP.
           10 KTY-BEFBED-TYP       PIC S9(4) USAGE COMP.
           10 KNR-BEFBED-TYP       PIC S9(4) USAGE COMP.
           10 MS-EH-EG-MAX-PZTR    PIC S999999999V9 USAGE COMP-3.
           10 MS-ANZ-PARTNER       PIC S9(4) USAGE COMP.
           10 MS-HAUPTB-ANZPARTN   PIC S9(4) USAGE COMP.
           10 MS-HAUPTB-EPARTN     PIC S999999999V9 USAGE COMP-3.
           10 MS-HAUPTB-ZTRPARTN   PIC S9(4) USAGE COMP.
110707     10 MS-HB-ANZP-ERSATZ    PIC S9(4) USAGE COMP.
110707     10 MS-HB-EINHP-ERSATZ   PIC S999999999V9 USAGE COMP-3.
110707     10 MS-HB-MAXP-ERSATZ    PIC S9(4) USAGE COMP.
110707     10 MS-ANZPART-AB-RGS    PIC S9(4) USAGE COMP.
           10 MS-LAST-UPDATE       PIC X(26).
061204     10 MS-PROV-STF-AUSF     PIC S999V99 USAGE COMP-3.
150515     10 MS-EH-NEU-GES-PZ     PIC S9(09)V9 COMP-3.
150515     10 MS-EH-NEU-EG-NP-PZ   PIC S9(09)V9 COMP-3.
150515     10 MS-ABZPROZ-REG-NP    PIC S999V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER DRVD0012 *****************************
      *

