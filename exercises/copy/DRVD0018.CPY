      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  09.11.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVD0018.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVD0018.CPv  $
      *
      *   Rev 2.1   Dec 07 2004 16:37:32   t13ex43
      *AVD00001: PGM - VBI-SST : Entladen MS + MSBZ
      *
      *   Rev 2.0   Nov 09 2004 16:36:38   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVD0018  VBI-SCHNITTSTELLEN-INFOSATZ         *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    09.11.2004                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------------------------------------------------------------*
      * AVD00001!09.11.04!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DEN AUFBAU DES VBI-SCHNITTSTELLEN-     *
      *    INFOSATZES.                                                *
      *                                                               *
      *****************************************************************
      *
       01  SST-INFO-SATZ.
           10 SST-NAME             PIC X(008).
           10 SST-BEGINN           PIC X(026).
           10 SST-ENDE             PIC X(026).
           10 SST-ANZ-SAETZE       PIC Z(008)9.
           10 SST-JOB-NAME         PIC X(008).
      *
      ******* ENDE COPY-MEMBER DRVD0018 *****************************
      *
