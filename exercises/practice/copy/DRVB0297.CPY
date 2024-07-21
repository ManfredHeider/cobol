      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0297.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0297.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   26 Aug 1998 10:32:02   T13AE35
      *AAS00049 - Neuerstellung, PI/PO fuer ZWVB0016
      *
      *   Rev 1.0   23 Jul 1997 12:28:04   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0297                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!06.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0297                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0297.
          05 PI-DRVB0297-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0297.

          05 PO-DRVB0297-RC                 PIC  X(003).
             88 PO-DRVB0297-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0297-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0297-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0297-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0297-DATEN.
             10 PO-DRVB0297-IA-DAT-VON       PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0297 *****************************
