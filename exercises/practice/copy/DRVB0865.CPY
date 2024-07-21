      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  30.08.2007
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2007 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0865.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:/mf/CPY/DRVB0865.CPv  $
      *
      *   Rev 2.3   05 Oct 2007 13:52:40   t13ae35
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung Sch
      *reibmodul
      *
      *   Rev 2.2   04 Sep 2007 14:32:02   t13ex57
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung Sch
      *reibmodul
      *
      *   Rev 2.1   03 Sep 2007 14:45:50   t13ex57
      *ARW00041: Einzug SW-Nutzungspauschale über VB-Abrechnung Sch
      *reibmodul
      *
      *   Rev 2.0   30 Aug 2007 14:36:28   t13ex57
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0865                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0865                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: U. Boll                                           *
      *    DATUM:    30.08.2007                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00041!30.08.07! 01 ! NEU                       !U. Boll   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0865
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0865.
           05  PI-DRVB0865-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0865-DATUM-VON          PIC  X(010).
           05  PI-DRVB0865-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0865.
           05  PO-DRVB0865-RC                 PIC  X(003).
               88 PO-DRVB0865-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0865-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0865-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0865-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0865-PV-GS              PIC  S9(009)V99 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************


