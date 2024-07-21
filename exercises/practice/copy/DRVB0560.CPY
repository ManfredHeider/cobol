      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0560.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0560.CPv  $
      *
      *   Rev 2.1   22 Oct 2001 11:28:48   t13ae13
      *AVB00534: 2. SST für VB-Portal
      *
      *   Rev 2.0   15 May 2001 13:33:38   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00534!19.10.01! 01 ! NEUERSTELLUNG             ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0560                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0560.
          05 PI-DRVB0560-DATEN.
             10 PI-DRVB0560-MA-ST-NR         PIC S9(009)
                                             BINARY.
      *
       01 PO-DRVB0560.
          05 PO-DRVB0560-RC                  PIC  X(003).
             88 PO-DRVB0560-RC-OK            VALUE 'OK '.
             88 PO-DRVB0560-RC-NFD           VALUE 'NFD'.
          05 PO-DRVB0560-DATEN.
             10 PO-DRVB0560-MA-BDV-MITGLIED  PIC  X(01).
      ******** ENDE COPY-MEMBER **************************************
