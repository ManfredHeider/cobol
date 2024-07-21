      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0338.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0338.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Dec 1998 10:54:42   T13AE42
      *AAS00079 - Neuerstellung wg. Delta-Rueckkonvertierung
      *
      *   Rev 1.0   27 Oct 1998 16:35:18   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0338                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0010                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    04.11.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00079!04.11.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0338                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0338.
          05 PI-DRVB0338-PROGRAMM           PIC  X(008).
          05 PI-DRVB0338-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0338-BVH-BETR-NR        PIC  S9(009).
      *
       01 PO-DRVB0338.

          05 PO-DRVB0338-RC                 PIC  X(003).
             88 PO-DRVB0338-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0338-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0338-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0338-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0338-DATEN.
             10 PO-DRVB0338-BVH-UNTST-NR     PIC  S9(009) COMP.
             10 PO-DRVB0338-BVH-DAT-VON      PIC  X(010).
             10 PO-DRVB0338-MS-NR            PIC  S9(004) COMP.
             10 PO-DRVB0338-BS-NR            PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0338 *****************************
