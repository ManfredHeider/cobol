      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  10.09.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0953.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0953.CPv  $
      *
      *   Rev 2.2   Sep 29 2010 11:10:24   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - VB29, XA20
      *
      *   Rev 2.1   Oct 29 2009 13:19:48   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.0   Sep 10 2009 12:09:16   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0953                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0003                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: Walb                                             *
      *    DATUM:    06.07.2009                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB01040!10.09.09!2.1 ! NEU                       !Walb      *
      * AVB01031!24.03.10!2.2 ! Neue View VIVBB003        !Walb      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0953                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0953.
          05 PI-DRVB0953-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0953-MA-ST-NR              PIC S9(009)  COMP.

      *
       01 PO-DRVB0953.
          05 PO-DRVB0953-RC                    PIC  X(003).
             88 PO-DRVB0953-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0953-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0953-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0953-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0953-DATEN.
             10 PO-DRVB0953-ED-ST-NR           PIC S9(9)   USAGE COMP.
240310*      10 PO-DRVB0953-ED-GESAMT          PIC X(155).
240310       10 PO-DRVB0953-ED-GESAMT          PIC X(161).
      *
      ***** ENDE COPY-MEMBER DRVB0953 *****************************
