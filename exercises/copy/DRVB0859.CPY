      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0859.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0859.CPv  $
      *
      *   Rev 2.1   17 Jul 2007 08:06:54   t13ae13
      *AVB00923: neue Aufstiegsbedingungen DVAG - XA02
      *
      *   Rev 2.0   02 Jul 2007 12:20:32   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB0859                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0859                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    02.07.2007                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00923!02.07.07!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0859
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0859.
           05  PI-DRVB0859-MS-NR              PIC  S9(004)    COMP.
           05  PI-DRVB0859-KNR-GESELL-TYP     PIC  S9(004)    COMP.
      *
       01  PO-DRVB0859.
           05  PO-DRVB0859-RC                 PIC  X(003).
               88 PO-DRVB0859-RC-OK                VALUE 'OK '.
               88 PO-DRVB0859-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0859-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB0859-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB0859-MS-EH-EIG-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0859-MS-EH-GRU-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0859-MS-EH-GES-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB0859-MS-ZEITRAUM           PIC S9(004)     COMP.
           05 PO-DRVB0859-MS-EQF-EIG            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0859-MS-EQF-GRU            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0859-MS-EQF-GES            PIC S9(003)V99  COMP-3.
           05 PO-DRVB0859-MS-EH-EG-MAX-PZTR     PIC S9(09)V9    COMP-3.
           05 PO-DRVB0859-MS-ANZ-PARTNER        PIC S9(4)       COMP.
           05 PO-DRVB0859-MS-HAUPTB-ANZPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0859-MS-HAUPTB-EPARTN      PIC S9(09)V9    COMP-3.
           05 PO-DRVB0859-MS-HAUPTB-ZTRPARTN    PIC S9(4)       COMP.
           05 PO-DRVB0859-MS-HB-ANZP-ERSATZ     PIC S9(4)       COMP.
           05 PO-DRVB0859-MS-HB-EINHP-ERSATZ    PIC S9(09)V9    COMP-3.
           05 PO-DRVB0859-MS-HB-MAXP-ERSATZ     PIC S9(4)       COMP.
      *
      ******* ENDE COPY-MEMBER **************************************
