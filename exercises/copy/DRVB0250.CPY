      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0250.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0250.cpv  $
      *
      *   Rev 2.1   Nov 02 1999 12:29:52   T13AE42
      *AOP00177 - neu Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   23 Jul 1997 12:21:58   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0250                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0016                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    02.11.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!02.11.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0250                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0250.
          05 PI-DRVB0250-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0250-ZEITPUNKT-BIS      PIC  X(010).
          05 PI-DRVB0250-ZEITPUNKT-VON      PIC  X(010).

      *
       01 PO-DRVB0250.

          05 PO-DRVB0250-RC                 PIC  X(003).
             88 PO-DRVB0250-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0250-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0250-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0250-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0250-DATEN.
             10 PO-DRVB0250-MS-NR          PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0250 *****************************
