      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  05.03.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0843.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0843.CPv  $
      *
      *   Rev 2.1   30 Mar 2007 10:00:44   t13ae27
      *AAS00679: Jahreswettbewerb 2007 ...
      *
      *   Rev 2.0   Mar 05 2007 08:48:00   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0843                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0843                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Duerr                                          *
      *    DATUM:    05.03.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00679!05.03.07!    ! NEU                       ! Duerr    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0843
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0843.
           05  PI-DRVB0843-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0843-DATUM-VON          PIC  X(010).
           05  PI-DRVB0843-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0843.
           05  PO-DRVB0843-RC                 PIC  X(003).
               88 PO-DRVB0843-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0843-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0843-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0843-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0843-EH-GS-GR-EG-KOMP    PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0843-EH-GS-GR-KOMP       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0843-EH-GS-EG-KOMP       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0843-EH-NEU-GS-EG-KOMP   PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

