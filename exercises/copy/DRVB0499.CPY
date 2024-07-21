      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0499.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0499.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 10:26:58   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   19 May 2000 09:50:08   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0499                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0499                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    04.09.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!04.09.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0499
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0499.
           10 PI-DRVB0499-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0499-DAT-VON-EUR         PIC  X(010).
           10 PI-DRVB0499-DAT-BIS-EUR         PIC  X(010).
           10 PI-DRVB0499-MS-NR-NEU           PIC  S9(004) COMP.
      *
       01  PO-DRVB0499.
           05  PO-DRVB0499-RC                 PIC  X(003).
               88 PO-DRVB0499-RC-OK                VALUE 'OK '.
               88 PO-DRVB0499-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0499-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0499-NET-GES-INV            PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0499-NET-GES-INV-GESAMT     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0499-NET-GES-GRENZE         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0499-NET-GES-GESAMT         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0499-NET-GES-BEREINIGT      PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

