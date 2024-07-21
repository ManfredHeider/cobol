      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0542.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0542.CPv  $
      *
      *   Rev 2.1   31 May 2001 10:40:10   t13ae27
      *neue Funktion
      *
      *   Rev 2.0   15 May 2001 13:33:22   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0542                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0006                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P. Dürr                                          *
      *    DATUM:    10.05.2001                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00274!14.05.01! 01 ! NEUERSTELLUNG             ! P.Dürr   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0542                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01  PI-DRVB0542.
           05  PI-DRVB0542-DIR-KEY.
               10 PI-DRVB0542-DIR-NR          PIC S9(004) BINARY.
      *
       01  PO-DRVB0542.
           05  PO-DRVB0542-RC                 PIC  X(003).
               88 PO-DRVB0542-RC-OK                        VALUE 'OK '.
               88 PO-DRVB0542-RC-NOT-FOUND                 VALUE 'NFD'.
           05  PO-DRVB0542-KTY-REGION         PIC  S9(004) BINARY.
           05  PO-DRVB0542-KNR-REGION         PIC  S9(004) BINARY.
      *
      ***** Ende-Copy-Member *****************************************

