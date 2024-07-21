      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0162.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0162.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:16   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   10 Jun 1998 12:06:42   T13EX30
      *AAS00042 - Neuerstellung Hr. Maderner, PUT fuer DELTA
      *
      *   Rev 1.0   21 Jul 1997 16:47:48   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0162                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0001                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MADERNER                                         *
      *    DATUM:    18.08.1997                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AFVB0370!18.08.97! 01 ! NEUERSTELLUNG             ! MADERNER *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0162                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0162.
          05 PI-DRVB0162-PROGRAMM           PIC  X(008).
          05 PI-DRVB0162-UFKT-CODE          PIC  9(002).
      *
       01 PO-DRVB0162.

          05 PO-DRVB0162-RC                 PIC  X(003).
             88 PO-DRVB0162-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0162-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0162-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0162-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0162-DATEN.
             10 PO-DRVB0162-ST-NR           PIC  9(7)    COMP-3.
             10 PO-DRVB0162-KNR-ST-TYP      PIC S9(004)  BINARY.
      *
      ***** Ende Copy-Member DRVB0162 *****************************
      ***** Ende-Copy-Member *****************************************
