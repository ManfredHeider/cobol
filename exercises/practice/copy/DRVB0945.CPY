      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.07.2009
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2009 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0945.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0945.CPv  $
      *
      *   Rev 2.2   Sep 29 2010 11:10:18   t13ae13
      *AVB01031: ED- flexible Provisionszuordnung - VB29, XA20
      *
      *   Rev 2.1   Oct 29 2009 13:19:34   t13ae13
      *AVB01014: Änderung ED-Regelung ab Alter 60
      *
      *   Rev 2.0   Jul 06 2009 11:32:14   t13ae13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0945                                      *
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
      * AVB01014!06.07.09!2.1 ! NEU                       !Walb      *
      * AVB01031!24.03.10!2.2 ! Neue View VIVBB003        !Walb      *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0945                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0945.
          05 PI-DRVB0945-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0945-ED-ZWEIG              PIC S9(009)  COMP.

      *
       01 PO-DRVB0945.
          05 PO-DRVB0945-RC                    PIC  X(003).
             88 PO-DRVB0945-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0945-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0945-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0945-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0945-DATEN.
             10 PO-DRVB0945-ED-ST-NR           PIC S9(9)   USAGE COMP.
240310*      10 PO-DRVB0945-ED-GESAMT          PIC X(155).
240310       10 PO-DRVB0945-ED-GESAMT          PIC X(161).
      *
      ***** ENDE COPY-MEMBER DRVB0945 *****************************
