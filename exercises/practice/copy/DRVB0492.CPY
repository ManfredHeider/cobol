      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0492.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0492.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 09:35:08   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   19 May 2000 09:50:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0492                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0492                    *
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
      *    FÜR DIE FUNKTION DRVB0492
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0492.
           05  PI-DRVB0492-ST-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0492-DATUM-VON          PIC  X(010).
           05  PI-DRVB0492-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0492.
           05  PO-DRVB0492-RC                 PIC  X(003).
               88 PO-DRVB0492-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0492-RC-NOK                       VALUE 'NOK'.
               88 PO-DRVB0492-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0492-RC-ERR                       VALUE 'ERR'.
      *
           05  PO-DRVB0492-EH-GS-GR-EG-INV     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0492-EH-GS-GR-INV        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0492-EH-GS-EG-INV        PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0492-EH-NEU-GS-EG-INV    PIC  S9(009)V9 COMP-3.
      *
      ******* ENDE COPY-MEMBER **************************************

