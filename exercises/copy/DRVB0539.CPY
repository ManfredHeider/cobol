      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0539.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0539.CPv  $
      *
      *   Rev 2.1   23 Aug 2001 11:27:02   t13ae13
      *AVB00543: Riesterbereinigung
      *
      *   Rev 2.0   22 Feb 2001 16:59:56   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0539                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0539                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    28.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!28.06.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0539
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0539.
           05  PI-DRVB0539-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0539-DATUM-VON          PIC  X(010).
           05  PI-DRVB0539-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0539.
           05  PO-DRVB0539-RC                 PIC  X(003).
               88 PO-DRVB0539-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0539-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0539-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0539-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0539-EH-GS-GR-EG-PRG     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0539-EH-GS-GR-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0539-EH-GS-EG-PRG        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0539-EH-NEU-GS-EG-PRG    PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

