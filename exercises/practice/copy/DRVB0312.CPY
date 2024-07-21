      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0312.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0312.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:50   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Sep 1998 17:23:42   T13AE35
      *ASL00022 - Neuerstellung
      *
      *   Rev 1.0   09 Jul 1998 09:57:38   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0312                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    24.08.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00022!24.08.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0312                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0312.
          05 PI-DRVB0312-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0312-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0312.

          05 PO-DRVB0312-RC                 PIC  X(003).
             88 PO-DRVB0312-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0312-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0312-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0312-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0312-DATEN.
             10 PO-DRVB0312-MS-NR            PIC S9(004) COMP.
             10 PO-DRVB0312-BS-NR            PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0312 *****************************
