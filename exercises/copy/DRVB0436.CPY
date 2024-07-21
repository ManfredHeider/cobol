      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0436.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0436.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:43:02   T13AE27
      *neue Funktion für ZWVB0065
      *
      *   Rev 2.0   Aug 04 1999 10:30:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0436                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0436                    *
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
      *    FÜR DIE FUNKTION DRVB0436
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0436.
           05  PI-DRVB0436-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0436-DATUM-VON          PIC  X(010).
           05  PI-DRVB0436-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0436.
           05  PO-DRVB0436-RC                 PIC  X(003).
               88 PO-DRVB0436-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0436-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0436-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0436-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0436-EH-NEU-GS-GR-EG    PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0436-NI-EH-NEU-GS-GR-EG PIC  S9(004)   COMP.
      *
      ******* ENDE COPY-MEMBER **************************************

