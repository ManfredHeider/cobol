      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0490.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0490.CPv  $
      *
      *   Rev 2.1   13 Oct 2000 09:34:08   T13AE28
      *AVB00490: Ber. Inv. E in UM15 und XA02/Bild 1
      *
      *   Rev 2.0   19 May 2000 09:49:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
     *****************************************************************
      *    COPY-MEMBER: DRVB0490                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0490                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    26.06.2000                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00490!26.06.00!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0490
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0490.
           10 PI-DRVB0490-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0490-DAT-VON             PIC  X(010).
           10 PI-DRVB0490-DAT-BIS             PIC  X(010).
           10 PI-DRVB0490-MS-NR-NEU           PIC  S9(004) COMP.
           10 PI-DRVB0490-VBP-NR-NEU          PIC  S9(004) COMP.
      *
       01  PO-DRVB0490.
           05  PO-DRVB0490-RC                 PIC  X(003).
               88 PO-DRVB0490-RC-OK                VALUE 'OK '.
               88 PO-DRVB0490-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0490-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0490-NET-GES-INV            PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-GES-INV-GESAMT     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-GES-GRENZE         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-GES-BEREINIGT      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-NEU-EG-INV         PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-NEU-EG-INV-GES     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-NEU-EG-GRENZE      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0490-NET-NEU-EG-BEREIN      PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************

