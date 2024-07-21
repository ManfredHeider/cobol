      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0369.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0369.CPv  $
      *
      *   Rev 2.1   Apr 27 1999 17:00:16   T13AE42
      * 
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.0   22 Jan 1999 13:41:12   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0369                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0049                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    18.03.1999                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00127!18.03.99! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0369                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0369.
          05 PI-DRVB0369-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0369-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0369.

          05 PO-DRVB0369-RC                 PIC  X(003).
             88 PO-DRVB0369-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0369-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0369-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0369-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0369-DATEN.
             10 PO-DRVB0369-MAT-AUS-GRND     PIC S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0369 *****************************
