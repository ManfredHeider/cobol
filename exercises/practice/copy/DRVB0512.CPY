      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0512.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0512.CPv  $
      *
      *   Rev 2.1   23 Jan 2001 12:03:02   T13AE28
      *AVB00480: Ber.Eig.E für RD1-D
      *
      *   Rev 2.0   07 Sep 2000 11:32:16   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
     *****************************************************************
      *    COPY-MEMBER: DRVB0512                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0512                    *
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
      *    FÜR DIE FUNKTION DRVB0512
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0512.
           10 PI-DRVB0512-MA-ST-NR            PIC  S9(009) COMP.
           10 PI-DRVB0512-DAT-VON             PIC  X(010).
           10 PI-DRVB0512-DAT-BIS             PIC  X(010).
           10 PI-DRVB0512-MS-NR-NEU           PIC  S9(004) COMP.
      *
       01  PO-DRVB0512.
           05  PO-DRVB0512-RC                 PIC  X(003).
               88 PO-DRVB0512-RC-OK                VALUE 'OK '.
               88 PO-DRVB0512-RC-NOK               VALUE 'NOK'.
               88 PO-DRVB0512-RC-NOT-FOUND         VALUE 'NFD'.
      *
           05  PO-DRVB0512-NET-EG-GESAMT      PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0512-NET-EG-ABZUG       PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0512-NET-GES-GESAMT     PIC  S9(009)V9 COMP-3.
           05  PO-DRVB0512-NET-GES-BEREINIGT  PIC  S9(009)V9 COMP-3.
      ******* ENDE COPY-MEMBER **************************************
