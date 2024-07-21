      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0404.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0404.cpv  $
      *
      *   Rev 2.1   Jun 04 1999 12:58:52   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:55:28   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0404                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0016                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    31.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!31.05.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0404                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0404.
          05 PI-DRVB0404-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0404-ZEITPUNKT-BIS      PIC  X(010).
          05 PI-DRVB0404-ZEITPUNKT-VON      PIC  X(026).

      *
       01 PO-DRVB0404.

          05 PO-DRVB0404-RC                 PIC  X(003).
             88 PO-DRVB0404-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0404-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0404-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0404-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0404-DATEN.
             10 PO-DRVB0404-MS-NR          PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0404 *****************************
