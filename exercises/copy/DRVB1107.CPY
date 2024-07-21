      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.06.2015
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *    Copyright (c) 2015 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1107.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1107.CPv  $
      *
      *   Rev 2.1   Jul 08 2015 12:33:22   t13ae13
      *AVB01332: neues Karrieresystem Allfinanz - VB22
      *
      *   Rev 2.0   Jun 10 2015 13:39:12   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      ****************************************************************
      *    COPY-MEMBER: DRVB1107                                     *
      *    --------------------------------------------------------  *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1107                   *
      *    --------------------------------------------------------  *
      *    ERSTELLT: WALB                                            *
      *    DATUM:    16.06.2015                                      *
      ****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01332!16.06.15!2.1 ! NEU                       ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1107
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB1107.
           05  PI-DRVB1107-MS-NR              PIC  S9(004)    COMP.
           05  PI-DRVB1107-KNR-GESELL-TYP     PIC  S9(004)    COMP.
           05  PI-DRVB1107-KNR-BEFBED-TYP     PIC  S9(004)    COMP.
      *
       01  PO-DRVB1107.
           05  PO-DRVB1107-RC                 PIC  X(003).
               88 PO-DRVB1107-RC-OK                VALUE 'OK '.
               88 PO-DRVB1107-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB1107-RC-NOT-FOUND         VALUE 'NFD'.
               88 PO-DRVB1107-RC-ERR               VALUE 'ERR'.

           05 PO-DRVB1107-MS-EH-EIG-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB1107-MS-EH-GES-PZTR        PIC S9(009)V9   COMP-3.
           05 PO-DRVB1107-MS-ZEITRAUM           PIC S9(004)     COMP.
           05 PO-DRVB1107-MS-EQF-EIG            PIC S9(003)V99  COMP-3.
           05 PO-DRVB1107-MS-EQF-GES            PIC S9(003)V99  COMP-3.
           05 PO-DRVB1107-MS-EH-EG-MAX-PZTR     PIC S9(09)V9    COMP-3.
           05 PO-DRVB1107-MS-EH-NEU-GES-PZ      PIC S9(09)V9    COMP-3.
           05 PO-DRVB1107-MS-EH-NEU-EG-NP-PZ    PIC S9(09)V9    COMP-3.
           05 PO-DRVB1107-MS-ABZPROZ-REG-NP     PIC S9(003)V99  COMP-3.

      *
      ******* ENDE COPY-MEMBER **************************************

