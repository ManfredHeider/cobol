      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.08.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0013.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0013.CPv  $
      *
      *   Rev 2.1   Sep 20 2004 10:04:40   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Aug 10 2004 09:19:58   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVD0013  VBI-SCHNITTSTELLE STVD0024          *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    10.08.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVD00001!10.08.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DER VBI-SCHNITTSTELLE       *
      *    STVD0024 - MITARBEITERSTUFEN-BONUSSTUFENZUORDNUNG (MSBZ).  *
      *                                                               *
      *****************************************************************
      *
       01  STVD0024-SATZ.
           10 MA-ST-NR             PIC S9(9) USAGE COMP.
           10 MS-NR-K              PIC S9(9) USAGE COMP.
           10 MS-NR                PIC S9(4) USAGE COMP.
           10 BS-NR                PIC S9(4) USAGE COMP.
           10 MSBZ-DAT-VON         PIC X(26).
           10 MSBZ-LAST-UPDATE     PIC X(26).
           10 MSBZ-DAT-BIS         PIC X(10).
      *
      ******* ENDE COPY-MEMBER DRVD0013 *****************************
      *
