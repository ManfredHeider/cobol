      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0867.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0867.CPv  $
      *
      *   Rev 2.1   01 Nov 2007 10:14:20   t13ae35
      *ASL00270: Dimension 7 Zukunft - BOZ
      *
      *   Rev 2.0   27 Sep 2007 14:19:10   t13ae35
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0867                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0867                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: J.Hering                                         *
      *    DATUM:    26.09.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00272!27.09.07! 2.1! NEU                       ! J.Hering *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0867
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0867.
           05  PI-DRVB0867-ST-NR              PIC  S9(009) COMP.
           05  PI-DRVB0867-DATUM-VON          PIC  X(010).
           05  PI-DRVB0867-DATUM-BIS          PIC  X(010).
           05  PI-DRVB0867-SPARTE             PIC  S9(004) COMP.
      *
       01  PO-DRVB0867.
           05  PO-DRVB0867-RC                 PIC  X(003).
               88 PO-DRVB0867-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0867-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0867-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0867-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0867-EH-GS-GS        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0867-EH-GS-GR        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0867-EH-GS-EG        PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


