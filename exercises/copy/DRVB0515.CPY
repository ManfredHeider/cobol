      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0515.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/MF/CPY/DRVB0515.CPv  $
      *
      *   Rev 2.1   27 Feb 2002 09:33:44   t13ae13
      *ASL00068: Anpassung OLZ-I an Partnerbedingungen OLZ-III
      *
      *   Rev 2.0   07 Sep 2000 11:32:20   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0515                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0019                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    08.02.01                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00068!08.02.00! 01 ! NEUERSTELLUNG             ! WALB     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0515                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0515.
          05 PI-DRVB0515-PROGRAMM           PIC  X(008).
          05 PI-DRVB0515-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0515-KNR-GESELL-TYP     PIC  S9(004).
          05 PI-DRVB0515-MS-NR-AB           PIC  S9(004).
          05 PI-DRVB0515-MS-NR-BIS          PIC  S9(004).
          05 PI-DRVB0515-ZEITPUNKT          PIC  X(010).
      *
       01 PO-DRVB0515.

          05 PO-DRVB0515-RC                 PIC  X(003).
             88 PO-DRVB0515-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0515-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0515-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0515-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0515-DATEN.
             10 PO-DRVB0515-MA-ST-NR         PIC  S9(009) COMP.
      *
      ***** ENDE COPY-MEMBER DRVB0515 *****************************

