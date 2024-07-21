      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  29.02.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0900.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0900.CPv  $
      *
      *   Rev 2.1   Mar 10 2008 08:10:52   t13ae13
      *AAS00744: Siegeradler 2008
      *
      *   Rev 2.0   29 Feb 2008 10:42:06   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0900                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0900                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    29.02.2008                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00744!29.02.08!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0900
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0900.
           05  PI-DRVB0900-MS-NR              PIC  S9(004)    COMP.
           05  PI-DRVB0900-KNR-GESELL-TYP     PIC  S9(004)    COMP.
           05  PI-DRVB0900-KNR-BEFBED-TYP     PIC  S9(004)    COMP.
      *
       01  PO-DRVB0900.
           05  PO-DRVB0900-RC                 PIC  X(003).
               88 PO-DRVB0900-RC-OK                VALUE 'OK '.
               88 PO-DRVB0900-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0900-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0900-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB0900-MS-EH-EIG-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0900-MS-EH-GRU-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0900-MS-EH-GES-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0900-MS-ZEITRAUM           PIC S9(004)     COMP.
           05 PO-DRVB0900-MS-EQF-EIG            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0900-MS-EQF-GRU            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0900-MS-EQF-GES            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0900-MS-EH-EG-MAX-PZTR     PIC S9(09)V9    COMP-3.
           05 PO-DRVB0900-MS-ANZ-PARTNER        PIC S9(4)       COMP.
           05 PO-DRVB0900-MS-HAUPTB-ANZPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0900-MS-HAUPTB-EPARTN      PIC S9(09)V9    COMP-3.
           05 PO-DRVB0900-MS-HAUPTB-ZTRPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0900-MS-HB-ANZP-ERSATZ     PIC S9(4)       COMP.
           05 PO-DRVB0900-MS-HB-EINHP-ERSATZ    PIC S9(09)V9    COMP-3.
           05 PO-DRVB0900-MS-HB-MAXP-ERSATZ     PIC S9(4)       COMP.
           05 PO-DRVB0900-MS-ANZPART-AB-RGS     PIC S9(4)       COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
