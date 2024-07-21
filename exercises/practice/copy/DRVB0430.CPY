      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0430.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0430.CPv  $
      *
      *   Rev 2.1   Mar 03 2000 13:11:52   T13AE27
      *Neu erstellt
      *
      *   Rev 2.0   Jun 02 1999 14:44:58   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0430                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0430                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    22.02.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00047!22.02.00! 01 ! NEU                       ! Walb     *
240322* VLA-4369!24.03.22! 02 ! Felddefinition COMP-3     ! U. Heim  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0430
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0430.
           05  PI-DRVB0430-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0430-DATUM-VON          PIC  X(010).
           05  PI-DRVB0430-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0430.
           05  PO-DRVB0430-RC                 PIC  X(003).
               88 PO-DRVB0430-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0430-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0430-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0430-RC-ERR                       VALUE 'ERR'.
      *
240322     05  PO-DRVB0430-EH-GS-EG           PIC  S9(010)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


