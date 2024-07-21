      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0566.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0566.CPv  $
      *
      *   Rev 2.1   Apr 30 2002 09:14:34   t13ae27
      *zusätzliche Parameter in CTV-Briefen
      *
      *   Rev 2.0   15 May 2001 13:33:44   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0566                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0009                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: DUERR                                            *
      *    DATUM:    29.01.2002                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ACV00007!29.01.02! 01 ! NEUERSTELLUNG             ! DUERR    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0566                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0566.
          05 PI-DRVB0566-MS-NR              PIC S9(004) COMP.
      *
       01 PO-DRVB0566.

          05 PO-DRVB0566-RC                 PIC  X(003).
             88 PO-DRVB0566-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0566-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0566-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0566-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0566-DATEN.
             10 PO-DRVB0566-MS-BEZ-KURZ     PIC  X(004).
             10 PO-DRVB0566-MS-BEZ          PIC  X(040).

