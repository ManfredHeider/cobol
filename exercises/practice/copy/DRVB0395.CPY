      *---------------------------------------------------------------*
      *    Standard f?r alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0395.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/MF/CPY/DRVB0395.CPv  $
      *
      *   Rev 2.2   Mar 20 2002 14:21:34   t13ae35
      *FSL00091 - Betrag mit Vorzeichen definiert
      *
      *   Rev 2.1   Apr 26 2000 12:40:38   T13AE35
      *ASL00049 - neu, DIM7-LEISTUNGEN 
      *JE MONAT
      *
      *   Rev 2.0   Mar 17 1999 11:00:00   P13SP20
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0349                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0026                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    10.04.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00049!10.04.00! 01 ! NEUERSTELLUNG             ! Walb     *
      * -------------------------------------------------------------*
      * FSL00091!20.03.02! 02 ! Betrag mit VZ definiert   ! Hering   *
      * -------------------------------------------------------------*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0395                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0395.
          05 PI-DRVB0395-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0395-MONAT              PIC  9(002).
          05 PI-DRVB0395-JAHR               PIC  9(004).
      *
       01 PO-DRVB0395.

          05 PO-DRVB0395-RC                 PIC  X(003).
             88 PO-DRVB0395-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0395-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0395-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0395-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0395-DATEN.
             10 PO-DRVB0395-MA-ST-NR        PIC S9(009)  COMP.
             10 PO-DRVB0395-KNR-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0395-KTY-OLZ-TYP     PIC S9(004)  COMP.
             10 PO-DRVB0395-KNR-WAEHRUNG    PIC S9(004)  COMP.
             10 PO-DRVB0395-KTY-WAEHRUNG    PIC S9(004)  COMP.
200203*      10 PO-DRVB0395-OLZZ-BETRAG     PIC 9(011).
200203       10 PO-DRVB0395-OLZZ-BETRAG     PIC S9(011).
      *
      ***** ENDE COPY-MEMBER DRVB0395 *****************************
