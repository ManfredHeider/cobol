      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0453.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0453.CPv  $
      *
      *   Rev 2.1   Feb 23 2000 12:17:54   T13AE28
      * 
      *
      *   Rev 2.0   Feb 07 2000 17:17:18   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0453                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: D.Ehrmann                                        *
      *    DATUM:    22.02.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00201!22.02.00! 01 ! NEUERSTELLUNG             ! D.Ehrmann*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0453                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0453.
          05 PI-DRVB0453-PROGRAMM           PIC  X(008).
          05 PI-DRVB0453-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0453-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0453-ZEITPUNKT-VON      PIC  X(010).
          05 PI-DRVB0453-ZEITPUNKT-BIS      PIC  X(010).
      *
       01 PO-DRVB0453.

          05 PO-DRVB0453-RC                 PIC  X(003).
             88 PO-DRVB0453-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0453-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0453-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0453-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0453-DATEN.
             10 PO-DRVB0453-ED-KONTO-NR      PIC  S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0453 *****************************

