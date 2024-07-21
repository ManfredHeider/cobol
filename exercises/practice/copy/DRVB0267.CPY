      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0267.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0267.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:38   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:50   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:24:10   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0267                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0010                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Zgur-Vujic                                       *
      *    DATUM:    15.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.07.98! 01 ! NEUERSTELLUNG             !Zgur-Vujic*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0267                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0267.
          05 PI-DRVB0267-VBP-NR             PIC S9(004) COMP.
      *
       01 PO-DRVB0267.

          05 PO-DRVB0267-RC                 PIC  X(003).
             88 PO-DRVB0267-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0267-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0267-DATEN.
             10 PO-DRVB0267-VBP-PROV-STF    PIC  9(003)V9(002).
      *
      ***** ENDE COPY-MEMBER DRVB0267 *****************************
