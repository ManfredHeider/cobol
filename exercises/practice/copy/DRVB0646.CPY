      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0646.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0646.CPv  $
      *
      *   Rev 2.1   Mar 19 2003 08:46:34   t13ex22
      *AVB00648: Neuerstellung
      *
      *   Rev 2.0   Feb 17 2003 10:56:46   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0646                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    18.03.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00648!18.03.03! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0646                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0646.
          05 PI-DRVB0646-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0646-ZEITPUNKT          PIC X(010).
          05 PI-DRVB0646-MS-NR              PIC S9(004) COMP.

      *
       01 PO-DRVB0646.

          05 PO-DRVB0646-RC                 PIC  X(003).
             88 PO-DRVB0646-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0646-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0646-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0646-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0646-DATEN.
             10 PO-DRVB0646-MS-NR            PIC S9(004) COMP.
             10 PO-DRVB0646-BS-NR            PIC S9(004) COMP.
             10 PO-DRVB0646-MSBZ-DAT-VON     PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0646 *****************************
