      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2001
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 2001 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0526.CPY  $
      *    $Revision:   2.4  $
      *
      *    $Log:   L:\MF\CPY\DRVB0526.CPv  $
      *
      *   Rev 2.4   20 Jul 2001 13:42:22   t13ae30
      *FAS00068 - Rücksprung auf Rev 2.2
      *
      *   Rev 2.3   09 Jul 2001 07:45:02   t13ae13
      *FAS00068: neue PI-FELDER wg. Cursoränderung
      *
      *   Rev 2.2   21 Mar 2001 14:55:56   T13AE13
      *AAS00264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.1   20 Mar 2001 10:05:16   T13AE13
      *AAS000264 - Wettbewerb und Leistungsplan 2001
      *
      *   Rev 2.0   22 Feb 2001 16:59:48   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0526                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZJVB0020                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    06.03.01                                         *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AAS00264!06.03.01! 01 ! NEUERSTELLUNG             ! WALB     *
      * FAS00068!06.07.01! 02 ! weitere PI-Felder         ! WALB     *
      * FAS00068!20.07.01! 03 ! RÜCKSPRUNG AUF REV 2.2    ! TH.WILLIG*
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0526                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0526.
          05 PI-DRVB0526-PROGRAMM           PIC  X(008).
          05 PI-DRVB0526-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0526-BETR-NR            PIC  S9(009) COMP.
          05 PI-DRVB0526-MS-NR-AB           PIC  S9(004).
          05 PI-DRVB0526-ERM-MONAT          PIC  X(010).
      *
       01 PO-DRVB0526.

          05 PO-DRVB0526-RC                 PIC  X(003).
             88 PO-DRVB0526-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0526-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0526-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0526-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0526-DATEN.
             10 PO-DRVB0526-UNTST-NR         PIC  S9(009) COMP.
             10 PO-DRVB0526-UNTST-MS-NR      PIC  S9(004) COMP.
      *
      *********** ENDE COPY-MEMBER DRVB0526*****************************
