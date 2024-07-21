      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0330.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0330.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Feb 1999 14:19:42   T13AE42
      *ASL00025 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   27 Oct 1998 16:34:32   P13SP19
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0330                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0072                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    29.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00025!29.10.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0330                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0330.
          05 PI-DRVB0330-KNR-STEUER-INFO    PIC S9(004) COMP.
      *
       01 PO-DRVB0330.

          05 PO-DRVB0330-RC                 PIC  X(003).
             88 PO-DRVB0330-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0330-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0330-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0330-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0330-DATEN.
             10 PO-DRVB0330-ZEITPUNKT-STAT   PIC X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0330 *****************************
