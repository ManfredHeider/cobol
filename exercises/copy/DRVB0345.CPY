      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  November 1995
      *            Kopf aus:  N:\PROFILE\KOPFCBL.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1995 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0345.CPY  $
      *    $Revision:   2.0  $
      *
      *    $Log:   L:\MF\CPY\DRVB0345.CPv  $
      *
      *   Rev 2.0   Mar 09 1999 13:54:28   T13AE35
      *AAS00095 - neu
      *
      *   Rev 1.0   27 Oct 1998 16:56:40   P13SP20
      *Initial revision.
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0345                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0011                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.12.1998                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00095!08.12.98! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0345                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0345.
          05 PI-DRVB0345-PROGRAMM           PIC  X(008).
          05 PI-DRVB0345-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0345-MS-NR-VON          PIC  S9(004)   COMP.
          05 PI-DRVB0345-MS-NR-BIS          PIC  S9(004)   COMP.
          05 PI-DRVB0345-DIR-NR-VON         PIC  S9(004)   COMP.
          05 PI-DRVB0345-DIR-NR-BIS         PIC  S9(004)   COMP.
      *
       01 PO-DRVB0345.

          05 PO-DRVB0345-RC                 PIC  X(003).
             88 PO-DRVB0345-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0345-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0345-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0345-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0345-DATEN.
             10 PO-DRVB0345-MA-ST-NR         PIC  S9(009) COMP.
             10 PO-DRVB0345-MS-NR            PIC  S9(004) COMP.
             10 PO-DRVB0345-DIR-NR           PIC  S9(004) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0345 *****************************
