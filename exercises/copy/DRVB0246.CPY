      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0246.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0246.CPv  $
      *
      *   Rev 2.1   May 20 2008 12:11:30   t13ae48
      *AVB00875: Verlängerung der Adressfelder und Kontaktdaten - P
      *rogramme (JMS) - 3
      *
      *   Rev 2.0   Feb 27 1999 15:58:34   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   17 Jun 1998 15:47:54   T13EX30
      *AAS00049 - Neuerstellung fuer DELTA-Rueckkonvertierung
      *
      *   Rev 1.0   23 Jul 1997 12:21:28   P13SP99
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0246                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZWVB0015                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    02.06.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00049!02.06.98! 01 ! NEUERSTELLUNG             ! WALB     *
      * AVB00875!11.03.08! 02 ! TELEFON-/TELEFAX 14->26   ! MAESS    *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0246                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0246.
          05 PI-DRVB0246-MAAN-BEZUGS-NR     PIC S9(009) COMP.
          05 PI-DRVB0246-KNR-MAAN-TYP       PIC S9(004) COMP.
      *
       01 PO-DRVB0246.

          05 PO-DRVB0246-RC                 PIC  X(003).
             88 PO-DRVB0246-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0246-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0246-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0246-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0246-DATEN.
             10 PO-DRVB0246-MAAN-STRASSE     PIC  X(040).
             10 PO-DRVB0246-MAAN-PLZ-AKTUELL PIC  X(009).
             10 PO-DRVB0246-MAAN-ORT         PIC  X(040).
             10 PO-DRVB0246-AUSL             PIC  X(004).
110308*      10 PO-DRVB0246-MAAN-TELEFAX     PIC  X(014).
110308*      10 PO-DRVB0246-MAAN-TELEFON     PIC  X(014).
110308       10 PO-DRVB0246-MAAN-TELEFAX     PIC  X(026).
110308       10 PO-DRVB0246-MAAN-TELEFON     PIC  X(026).

      *
      ***** ENDE COPY-MEMBER DRVB0246 *****************************
