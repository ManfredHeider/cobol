      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0326.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0326.CPv  $
      *
      *   Rev 2.1   Feb 14 2005 10:51:06   t13ex13
      *AAS00532: Basiszahlen Einheiten neuer VB ab 01.07.2003 je Re
      *gion
      *
      *   Rev 2.0   Feb 27 1999 15:58:54   P13SP20
      *NT-Umstellung
      *
      *   Rev 1.1   15 Dec 1998 16:13:48   T13AE35
      *AAS00079 - Neu, PI/PO fuer ZXVB0012
      *
      *   Rev 1.0   09 Jul 1998 09:58:48   P13SP13
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0326                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    21.10.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00079!21.10.98! 01 ! NEUERSTELLUNG             ! WALB     *
      * AAS00532!31.01.05!2.1 ! ERGÄNZUNG STUFE           !GUTFLEISCH*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0326                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0326.
          05 PI-DRVB0326-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0326-DAT-VON            PIC  X(010).
          05 PI-DRVB0326-DAT-BIS            PIC  X(010).
310105    05 PI-DRVB0326-STUFE              PIC S9(004) COMP.
      *
       01 PO-DRVB0326.

          05 PO-DRVB0326-RC                 PIC  X(003).
             88 PO-DRVB0326-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0326-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0326-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0326-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0326-DATEN.
             10 PO-DRVB0326-MA-ST-NR        PIC S9(009)  COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0326 *****************************
