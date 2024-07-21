      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0435.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0435.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:40:14   T13AE27
      *neue Funktion für ZWVB0065
      *
      *   Rev 2.0   Aug 04 1999 10:30:06   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0435                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0435                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    12.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!12.08.99!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0435
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0435.
           05  PI-DRVB0435-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0435-DATUM-VON          PIC  X(010).
           05  PI-DRVB0435-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0435.
           05  PO-DRVB0435-RC                 PIC  X(003).
               88 PO-DRVB0435-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0435-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0435-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0435-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0435-EH-GS-GR-EG         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0435-NI-EH-GS-GR-EG      PIC  S9(004)   COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

