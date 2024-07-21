      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0256.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0256.CPv  $
      *
      *   Rev 2.1   May 20 2008 12:11:40   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.0   Feb 27 1999 15:58:36   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   12 Aug 1998 16:55:32   T13EX19
      *VB-SCHNITTSTELLE
      *
      *   Rev 1.0   23 Jul 1997 12:22:46   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0256                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: ZGUR-VUJIC                                       *
      *    DATUM:    15.07.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AKI00001!15.07.98! 01 ! NEUERSTELLUNG             !ZGUR-VUJIC*
      * AVB00875!11.03.08! 02 ! TELEFON-/TELEFAX 14->26   !J.MAESS   *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0256                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0256.
          05 PI-DRVB0256-MAAN-BEZUGS-NR     PIC S9(009) COMP.
          05 PI-DRVB0256-KNR-MAAN-TYP       PIC S9(004) COMP.
      *
       01 PO-DRVB0256.

          05 PO-DRVB0256-RC                 PIC  X(003).
             88 PO-DRVB0256-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0256-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0256-DATEN.
             10 PO-DRVB0256-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0256-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0256-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0256-AUSL             PIC  S9(4) USAGE COMP.
110308*      10 PO-DRVB0256-MAAN-TELEFAX     PIC  X(014).
110308*      10 PO-DRVB0256-MAAN-TELEFON     PIC  X(014).
110308       10 PO-DRVB0256-MAAN-TELEFAX     PIC  X(026).
110308       10 PO-DRVB0256-MAAN-TELEFON     PIC  X(026).

      *
      ***** ENDE COPY-MEMBER DRVB0256 *****************************
