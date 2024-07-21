      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0403.CPY  $
      *    $Revision:   2.3  $
      *
      *    $Log:   L:\MF\CPY\DRVB0403.CPv  $
      *
      *   Rev 2.3   Nov 02 1999 14:14:24   T13AE42
      *AOP00177 - Korrektur
      *
      *   Rev 2.2   Nov 02 1999 10:58:16   T13AE42
      *AOP00177 - Korrektur
      *
      *   Rev 2.1   Jun 04 1999 12:51:54   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:55:12   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0403                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
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
      *    FÜR DIE FUNKTION DRVB0403                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0403.
          05 PI-DRVB0403-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0403-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0403.

          05 PO-DRVB0403-RC                 PIC  X(003).
             88 PO-DRVB0403-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0403-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0403-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0403-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0403-DATEN.
             10 PO-DRVB0403-MS-NR          PIC S9(004) COMP.
             10 PO-DRVB0403-BS-NR          PIC S9(004) COMP.
             10 PO-DRVB0403-MSBZ-DAT-VON   PIC  X(026).
             10 PO-DRVB0403-MSBZ-DAT-BIS   PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0403 *****************************

