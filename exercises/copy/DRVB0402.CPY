      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0402.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0402.cpv  $
      *
      *   Rev 2.1   Jun 04 1999 12:44:24   T13AE42
      *AOP00177 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   May 27 1999 13:54:52   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0402                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0045                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    28.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00177!28.05.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0402                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0402.
          05 PI-DRVB0402-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0402-ED-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0402.

          05 PO-DRVB0402-RC                 PIC  X(003).
             88 PO-DRVB0402-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0402-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0402-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0402-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0402-DATEN.
             10 PO-DRVB0402-EDT-DAT-BIS     PIC  X(010).
             10 PO-DRVB0402-EDT-DAT-VON     PIC  X(026).
      *
      ***** ENDE COPY-MEMBER DRVB0402 *****************************


