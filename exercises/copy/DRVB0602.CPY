      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0602.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0602.CPv  $
      *
      *   Rev 2.1   Nov 15 2002 16:57:20   t13ae30
      *AVB00643 - Kapitäns-Club
      *
      *   Rev 2.0   Nov 05 2002 11:34:48   p13sp16
      *Archiverstellung
      ****************************************************************
      *    COPY-MEMBER: DRVB0602                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0602                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WILLIG                                          *
      *    DATUM:    05.11.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00643!05.11.02! 01 ! NEU                       ! WILLIG   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0602
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0602.
           05  PI-DRVB0602-MA-ST-NR           PIC  S9(009)    COMP.
           05  PI-DRVB0602-ANZ-PARTNER        PIC  S9(004)    COMP.
           05  PI-DRVB0602-ANZ-MONATE         PIC  S9(004)    COMP.
           05  PI-DRVB0602-ANZ-EINHEITEN      PIC  S9(009)V9  COMP-3.
           05  PI-DRVB0602-STUFE              PIC  S9(004)    COMP.
      *
       01  PO-DRVB0602.
           05  PO-DRVB0602-RC                 PIC  X(003).
               88 PO-DRVB0602-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0602-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0602-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0602-RC-ERR                       VALUE 'ERR'.
      *
      ******* ENDE COPY-MEMBER **************************************
