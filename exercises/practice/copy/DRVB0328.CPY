      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0328.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0328.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   20 Nov 1998 12:19:10   T13AE42
      *ASL00024 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:56   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0328                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    23.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00024!23.10.98! 01 ! NEUERSTELLUNG             ! SCHMIDT,B*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0328                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0328.
          05 PI-DRVB0328-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0328.

          05 PO-DRVB0328-RC                 PIC  X(003).
             88 PO-DRVB0328-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0328-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0328-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0328-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0328-DATEN.
             10 PO-DRVB0328-MS-NR            PIC S9(004) COMP.
             10 PO-DRVB0328-BS-NR            PIC S9(004) COMP.
             10 PO-DRVB0328-MSBZ-DAT-BIS     PIC  X(010).
             10 PO-DRVB0328-MSBZ-DAT-VON     PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0328 *****************************
