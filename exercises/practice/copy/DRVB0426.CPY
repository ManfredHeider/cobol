      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0426.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0426.CPv  $
      *
      *   Rev 2.1   Dec 21 1999 09:23:12   T13AE27
      *neu Funktion für ZXVB0011
      *
      *   Rev 2.0   Jun 02 1999 14:44:56   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0426                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB0426                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    04.08.1999                                       *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00066!04.08.99!    ! NEU                       ! Walb     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB0426
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      *    >>>  SELECT ...                <<<
      *
      ****************************************************************
      *
       01  PI-DRVB0426.
           05  PI-DRVB0426-UFKT-CODE          PIC  9(002).
           05  PI-DRVB0426-VB-NR              PIC  S9(009)    COMP.
           05  PI-DRVB0426-DATUM-VON          PIC  X(010).
           05  PI-DRVB0426-DATUM-BIS          PIC  X(010).
      *
       01  PO-DRVB0426.
           05  PO-DRVB0426-RC                 PIC  X(003).
               88 PO-DRVB0426-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0426-RC-LEER                      VALUE 'LER'.
               88 PO-DRVB0426-RC-NOT-FOUND                 VALUE 'NFD'.
               88 PO-DRVB0426-RC-EOF                       VALUE 'EOF'.
      *
           05  PO-DRVB0426-BVH-UNTST-NR       PIC  S9(009) COMP.
           05  PO-DRVB0426-BVH-DAT-VON        PIC  X(010).
           05  PO-DRVB0426-BVH-DAT-BIS        PIC  X(010).
      *
      ******* ENDE COPY-MEMBER **************************************



