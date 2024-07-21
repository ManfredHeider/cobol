      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  14.04.2005
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2005 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0774.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0774.CPv  $
      *
      *   Rev 2.1   May 23 2005 10:17:34   t13ae27
      *AAS00531: JubiläumsWB2005 der DVAG Wien
      *
      *   Rev 2.0   Apr 14 2005 09:55:20   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0774                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0012                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    03.06.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!03.06.03! 01 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0774                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0774.
          05 PI-DRVB0774-PROGRAMM           PIC  X(008).
          05 PI-DRVB0774-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0774-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0774-WB-TYP             PIC S9(004) COMP.
          05 PI-DRVB0774-WERTUNGS-TYP       PIC S9(004) COMP.
          05 PI-DRVB0774-GESELL             PIC S9(004) COMP.
          05 PI-DRVB0774-DAT-VON            PIC  X(010).
          05 PI-DRVB0774-DAT-BIS            PIC  X(010).
      *
       01 PO-DRVB0774.

          05 PO-DRVB0774-RC                 PIC  X(003).
             88 PO-DRVB0774-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0774-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0774-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0774-RC-NOK                       VALUE 'NOK'.
             88 PO-DRVB0774-RC-ERR                       VALUE 'ERR'.

          05 PO-DRVB0774-DATEN.
             10 PO-DRVB0774-MONAT           PIC   X(010).
             10 PO-DRVB0774-EH              PIC  S9(009)V99 COMP-3.
      *
      ***** ENDE-COPY-MEMBER *****************************************
