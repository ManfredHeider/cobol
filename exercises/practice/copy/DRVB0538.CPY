      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0538.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0538.CPv  $
      *
      *   Rev 2.1   23 Aug 2001 11:24:20   t13ae13
      *AVB00543: Riesterbereinigung
      *
      *   Rev 2.0   22 Feb 2001 16:59:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     *****************************************************************
      *    COPY-MEMBER: DRVB0538                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0538                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    26.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00538!26.06.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0538
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0538.
           10 PI-DRVB0538-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0538-DAT-VON             PIC  X(010).
           10 PI-DRVB0538-DAT-BIS             PIC  X(010).
           10 PI-DRVB0538-MS-NR-NEU           PIC  S9(004) COMP.
           10 PI-DRVB0538-VBP-NR-NEU          PIC  S9(004) COMP.
      *
       01  PO-DRVB0538.
           05  PO-DRVB0538-RC                 PIC  X(003).
               88 PO-DRVB0538-RC-OK                VALUE 'OK '.
               88 PO-DRVB0538-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0538-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0538-NET-GES-PRG            PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-GES-PRG-GESAMT     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-GES-GRENZE         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-GES-BEREINIGT      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-NEU-EG-PRG         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-NEU-EG-PRG-GES     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-NEU-EG-GRENZE      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0538-NET-NEU-EG-BEREIN      PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

