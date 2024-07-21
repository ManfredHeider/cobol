      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0484.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0484.CPv  $
      *
      *   Rev 2.1   Jan 14 2003 09:28:36   t13ae13
      *AVB00649: Neues Aufstiegssystem für RGS - D
      *
      *   Rev 2.0   19 May 2000 09:49:46   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     *****************************************************************
      *    COPY-MEMBER: DRVB0484                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0484                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    20.11.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00480!20.11.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0484
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0484.
           10 PI-DRVB0484-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0484-DAT-VON             PIC  X(010).
           10 PI-DRVB0484-DAT-BIS             PIC  X(010).
           10 PI-DRVB0484-MS-NR-NEU           PIC  S9(004) COMP.
           10 PI-DRVB0484-MS-EH-EG-MAX        PIC  S9(009)V9 COMP-3.
      *
       01  PO-DRVB0484.
           05  PO-DRVB0484-RC                 PIC  X(003).
               88 PO-DRVB0484-RC-OK                VALUE 'OK '.
               88 PO-DRVB0484-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0484-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0484-NET-EG-GESAMT      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0484-NET-EG-ABZUG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0484-NET-GES-GESAMT     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0484-NET-GES-BEREINIGT  PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************
