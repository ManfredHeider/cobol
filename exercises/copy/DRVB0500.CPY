      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0500.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0500.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 10:27:42   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   19 May 2000 09:50:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0500                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0500                    *
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
      *    FÜR DIE FUNKTION DRVB0500
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0500.
           10 PI-DRVB0500-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0500-VBP-NR-NEU          PIC  S9(004) COMP.
           10 PI-DRVB0500-DAT-BIS-EUR         PIC  X(010).
      *
       01  PO-DRVB0500.
           05  PO-DRVB0500-RC                 PIC  X(003).
               88 PO-DRVB0500-RC-OK                VALUE 'OK '.
               88 PO-DRVB0500-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0500-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0500-NET-NEU-EG-INV         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0500-NET-NEU-EG-INV-GES     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0500-NET-NEU-EG-GRENZE      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0500-NET-NEU-EG-GESAMT      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0500-NET-NEU-EG-BEREIN      PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

