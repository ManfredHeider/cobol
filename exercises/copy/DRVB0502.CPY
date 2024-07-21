      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0502.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0502.CPv  $
      *
      *   Rev 2.1   29 Sep 2000 12:13:50   T13AE35
      *ASL00070- neu; in PI/PO fuer ZJVB0015
      *in letzten 12 Monaten herausgewachsene
      *Direktionen
      *
      *   Rev 2.0   07 Sep 2000 11:32:00   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0502                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: S. Walb                                          *
      *    DATUM:    20.09.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00070!20.09.00! 01 ! NEUERSTELLUNG             ! S. Walb  *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0502                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0502.
          05 PI-DRVB0502-PROGRAMM           PIC  X(008).
          05 PI-DRVB0502-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0502-ED-BESITZER        PIC  S9(009) COMP.
          05 PI-DRVB0502-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0502.

          05 PO-DRVB0502-RC                 PIC  X(003).
             88 PO-DRVB0502-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0502-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0502-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0502-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0502-DATEN.
             10 PO-DRVB0502-ED-BETREUTER     PIC  S9(009) COMP.
             10 PO-DRVB0502-MS-NR            PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0502 *****************************
