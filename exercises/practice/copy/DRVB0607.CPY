      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0607.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0607.CPv  $
      *
      *   Rev 2.2   Aug 17 2005 09:11:26   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.1   Nov 15 2002 16:57:52   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.0   Nov 05 2002 11:35:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0607                                     *
      *    FUNKTION KAPITÄNS-CLUB KRITERIEN PRÜFEN                   *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    07.11.2002                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00643!07.11.02! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0607                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0607.
          05 PI-DRVB0607-PROGRAMM           PIC  X(008).
          05 PI-DRVB0607-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0607-MA-ST-NR           PIC  9(007).
      *
       01 PO-DRVB0607.
          05 PO-DRVB0607-RC                 PIC  X(003).
             88 PO-DRVB0607-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0607-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0607-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0607-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0607-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVB0607 *****************************
