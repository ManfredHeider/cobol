      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0381.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0381.CPv  $
      *
      *   Rev 2.1   Jan 14 2003 09:25:22   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.0   Mar 17 1999 10:59:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0381                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0381                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    03.12.2002                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00649!03.12.02!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0381
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0381.
           05  PI-DRVB0381-MS-NR              PIC  S9(004)    COMP.
           05  PI-DRVB0381-KNR-GESELL-TYP     PIC  S9(004)    COMP.
      *
       01  PO-DRVB0381.
           05  PO-DRVB0381-RC                 PIC  X(003).
               88 PO-DRVB0381-RC-OK                VALUE 'OK '.
               88 PO-DRVB0381-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0381-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0381-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB0381-MS-EH-EIG-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0381-MS-EH-GRU-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0381-MS-EH-GES-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0381-MS-ZEITRAUM           PIC S9(004)     COMP.
           05 PO-DRVB0381-MS-EQF-EIG            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0381-MS-EQF-GRU            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0381-MS-EQF-GES            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0381-MS-EH-EG-MAX-PZTR     PIC S9(09)V9    COMP-3.
           05 PO-DRVB0381-MS-ANZ-PARTNER        PIC S9(4)       COMP.
           05 PO-DRVB0381-MS-HAUPTB-ANZPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0381-MS-HAUPTB-EPARTN      PIC S9(09)V9    COMP-3.
           05 PO-DRVB0381-MS-HAUPTB-ZTRPARTN    PIC S9(4)       COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
