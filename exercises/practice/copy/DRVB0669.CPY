      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0669.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0669.CPv  $
      *
      *   Rev 2.1   Sep 17 2004 11:15:28   t13ex22
      *AAS00425: Vergleich Beförderungsbed. RGS - RD2
      *
      *   Rev 2.0   Feb 17 2003 10:58:58   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0669                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0669                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: BOLL                                            *
      *    DATUM:    19.08.2003                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00425!19.08.03!2.1 ! NEU                       ! BOLL     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0669
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0669.
           05  PI-DRVB0669-MS-NR              PIC  S9(004)    COMP.
           05  PI-DRVB0669-KNR-GESELL-TYP     PIC  S9(004)    COMP.
           05  PI-DRVB0669-KNR-BEFBED-TYP     PIC  S9(004)    COMP.
      *
       01  PO-DRVB0669.
           05  PO-DRVB0669-RC                 PIC  X(003).
               88 PO-DRVB0669-RC-OK                VALUE 'OK '.
               88 PO-DRVB0669-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0669-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0669-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB0669-MS-EH-EIG-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0669-MS-EH-GRU-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0669-MS-EH-GES-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0669-MS-ZEITRAUM           PIC S9(004)     COMP.
           05 PO-DRVB0669-MS-EQF-EIG            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0669-MS-EQF-GRU            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0669-MS-EQF-GES            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0669-MS-EH-EG-MAX-PZTR     PIC S9(09)V9    COMP-3.
           05 PO-DRVB0669-MS-ANZ-PARTNER        PIC S9(4)       COMP.
           05 PO-DRVB0669-MS-HAUPTB-ANZPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0669-MS-HAUPTB-EPARTN      PIC S9(09)V9    COMP-3.
           05 PO-DRVB0669-MS-HAUPTB-ZTRPARTN    PIC S9(4)       COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
