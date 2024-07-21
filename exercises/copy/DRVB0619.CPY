      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Mai 2002
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2002 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0619.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/cpy/DRVB0619.CPv  $
      *
      *   Rev 2.1   Jan 15 2003 09:31:50   t13ae27
      *Betreuerbonus
      *
      *   Rev 2.0   Nov 05 2002 11:36:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0619                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0007                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT,B.                                       *
      *    DATUM:    02.11.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00149!02.12.02! 01 ! NEUERSTELLUNG             ! Duerr, P.*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0619                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0619.
          05 PI-DRVB0619-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0619-ZEITPUNKT          PIC X(010).
      *
       01 PO-DRVB0619.

          05 PO-DRVB0619-RC                 PIC  X(003).
             88 PO-DRVB0619-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0619-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0619-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0619-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0619-DATEN.
             10 PO-DRVB0619-VBP-NR           PIC S9(004) COMP.
