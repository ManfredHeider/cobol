      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0318.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:/mf/CPY/DRVB0318.CPv  $
      *
      *   Rev 2.4   25 Jun 2018 15:29:08   t13ae50
      *AVB01479: Einführung Allfinanz AG im VB-System - Statistikpa
      *ket - Programmen(sla)
      *
      *   Rev 2.3   Jan 07 2015 11:23:48   t13ae13
      *AVB01321: Integration FVD - Rest (SWL)
      *
      *   Rev 2.2   Apr 22 2008 13:57:10   t13ex43
      *AVB00961: PGM - Statistiken FVD (bkl)
      *
      *   Rev 2.1   13 Nov 2007 10:53:50   t13ex43
      *AVB00905: PGM - Batchanpassungen für Allfinanz (bkl)
      *
      *   Rev 2.0   Feb 27 1999 15:58:52   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   07 Sep 1998 12:14:54   T13AE42
      *AAS00049 - Neuerstellung wegen Delta-Rueckkonvertierung
      *
      *   Rev 1.0   09 Jul 1998 09:58:14   P13SP13
      *Initial revision.
      *
      *****************************************************************
      *    COPY-MEMBER: DRVB0318                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0008                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT, B.                                      *
      *    DATUM:    26.08.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!26.08.98! 01 ! NEUERSTELLUNG             ! SCHMIDT  *
010108* AVB00905!01.01.08! 2.1! GESELLSCHAFT HINZU        ! KLEEMEIER*
010708* AVB00961!01.07.08! 2.2! FVD HINZU                 ! KLEEMEIER*
      * AVB01321!11.11.14! 2.3! FVD entfällt              ! S. Walb  *
220618* AVB01479!22.06.18! 2.4! ALAG HINZU                ! S.LAS    *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0318                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0318.
          05 PI-DRVB0318-PROGRAMM           PIC  X(008).
          05 PI-DRVB0318-UFKT-CODE          PIC  9(002).
010108    05 PI-DRVB0318-GESELLSCHAFT       PIC  X(004).
010108       88 PI-DRVB0318-GESELL-DVAG                  VALUE 'DVAG'.
010108       88 PI-DRVB0318-GESELL-ALLFINANZ             VALUE 'ALFI'.
220618       88 PI-DRVB0318-GESELL-ALAG                  VALUE 'ALAG'.
      *
       01 PO-DRVB0318.
 
 
          05 PO-DRVB0318-RC                 PIC  X(003).
             88 PO-DRVB0318-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0318-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0318-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0318-RC-ERR                       VALUE 'ERR'.
 
 
          05 PO-DRVB0318-DATEN.
             10 PO-DRVB0318-MS-NR           PIC  S9(004)  COMP.
             10 PO-DRVB0318-MA-ST-NR        PIC  S9(009)  COMP.
             10 PO-DRVB0318-MAN-NAME        PIC   X(040).
             10 PO-DRVB0318-MAN-VORNAME     PIC   X(020).
             10 PO-DRVB0318-MAN-GEBURTSDAT  PIC   X(010).
             10 PO-DRVB0318-MAN-TITEL-AKAD  PIC   X(010).
      *
      ***** ENDE COPY-MEMBER *****************************************
      *
 

