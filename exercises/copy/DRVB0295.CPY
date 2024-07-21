      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0295.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0295.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:46   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Aug 1998 12:25:00   T13AE34
      *AAN00167 - Vorvertragsnummer von Fr. Walb
      *
      *   Rev 1.0   23 Jul 1997 12:27:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0295                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0013                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    07.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!07.07.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0295                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0295.
          05 PI-DRVB0295-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0295.

          05 PO-DRVB0295-RC                 PIC  X(003).
             88 PO-DRVB0295-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0295-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0295-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0295-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0295-DATEN.
             10 PO-DRVB0295-AV-DAT-VON       PIC  X(010).
             10 PO-DRVB0295-AV-DAT-BIS       PIC  X(010).
      *
      ***** ENDE COPY-MEMBER DRVB0295 *****************************
