      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0213.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0213.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:26   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 May 1998 15:21:36   T13EX30
      *AAS00049 - Neuerstellung
      *
      *   Rev 1.0   23 Jul 1997 12:16:40   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0213                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0004                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    02.03.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00005!02.03.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0213                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0213.
          05 PI-DRVB0213-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0213-ZEITPUNKT-ISO      PIC  X(010).
      *
       01 PO-DRVB0213.

          05 PO-DRVB0213-RC                 PIC  X(003).
             88 PO-DRVB0213-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0213-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0213-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0213-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0213-DATEN.
             10 PO-DRVB0213-ST-NR           PIC S9(009)  COMP.
             10 PO-DRVB0213-DIR-NR          PIC S9(004)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0213 *****************************
