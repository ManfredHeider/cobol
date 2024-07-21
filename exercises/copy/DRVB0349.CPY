      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0349.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0349.CPv  $
      *
      *   Rev 2.0   Feb 27 1999 15:58:56   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   11 Jan 1999 12:20:06   T13AE42
      *ASL00036 - Neuerstellung
      *
      *   Rev 1.0   27 Oct 1998 16:57:04   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0026                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: SCHMIDT                                          *
      *    DATUM:    17.12.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ARW00017!17.12.98! 01 ! NEUERSTELLUNG             ! SCHMIDT,B.
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0349                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0349.
          05 PI-DRVB0349-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0349-MONAT              PIC  9(002).
          05 PI-DRVB0349-JAHR               PIC  9(004).
      *
       01 PO-DRVB0349.

          05 PO-DRVB0349-RC                 PIC  X(003).
             88 PO-DRVB0349-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0349-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0349-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0349-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0349-DATEN.
             10 PO-DRVB0349-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0349-KNR-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0349-KTY-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0349-KNR-WAEHRUNG    PIC S9(004)  COMP.
             10 PO-DRVB0349-KTY-WAEHRUNG    PIC S9(004)  COMP.
             10 PO-DRVB0349-OLZZ-BETRAG     PIC 9(011).
      *
      ***** ENDE COPY-MEMBER DRVB0349 *****************************
