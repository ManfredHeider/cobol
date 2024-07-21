      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   drvb0400.cpy  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\drvb0400.cpv  $
      *
      *   Rev 2.1   May 27 1999 16:46:34   T13AE42
      *AOP00170 - Delta-Rueckkonvertierung
      *
      *   Rev 2.0   Mar 17 1999 11:00:02   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0400                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0049                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    27.05.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AOP00170!27.05.99! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0400                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0400.
          05 PI-DRVB0400-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0400.

          05 PO-DRVB0400-RC                 PIC  X(003).
             88 PO-DRVB0400-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0400-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0400-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0400-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0400-DATEN.
             10 PO-DRVB0400-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0400-MAT-DAT-BIS     PIC  X(010).
             10 PO-DRVB0400-MAT-DAT-VON     PIC  X(026).
      *
      ***** ENDE COPY-MEMBER DRVB0400 *****************************

