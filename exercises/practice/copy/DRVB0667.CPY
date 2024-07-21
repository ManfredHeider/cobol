      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0667.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0667.CPv  $
      *
      *   Rev 2.2   Aug 17 2005 09:11:40   t13ae30
      *AVD00009: Projekt VBI: SST Clubprüfung
      *
      *   Rev 2.1   Aug 14 2003 14:10:52   t13ae30
      *AVB00681 - Club "Bes.Leist. im AS-Club"
      *
      *   Rev 2.0   Feb 17 2003 10:58:46   p13sp16
      *Archiverstellung
      *
      ****************************************************************
      *    COPY-MEMBER: DRVB0681                                     *
      *    FUNKTION BES.LEIST. IM AS-CLUB KRITERIEN PRÜFEN           *
      *                                                              *
      *    --------------------------------------------------------- *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    30.07.2003                                      *
      ****************************************************************
      *    VERSION   01                                              *
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00681!30.07.03! 01 ! NEUERSTELLUNG             ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *                                                              *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0667                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
       01 PI-DRVB0667.
          05 PI-DRVB0667-PROGRAMM           PIC  X(008).
          05 PI-DRVB0667-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0667-MA-ST-NR           PIC  9(007).
          05 PI-DRVB0667-AST-MONAT          PIC  X(010).
      *
       01 PO-DRVB0667.
          05 PO-DRVB0667-RC                 PIC  X(003).
             88 PO-DRVB0667-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0667-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0667-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0667-RC-NFD                       VALUE 'NFD'.
             88 PO-DRVB0667-RC-ERR                       VALUE 'ERR'.
      *
      ******** Ende Copy-Member DRVB0667 *****************************
