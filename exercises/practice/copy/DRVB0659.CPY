      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0659.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0659.CPv  $
      *
      *   Rev 2.1   Jun 05 2003 10:42:32   t13ex22
      *AVB00666: AL-Zusätze anzeigen
      *
      *   Rev 2.0   Feb 17 2003 10:58:00   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0659                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: MUND                                             *
      *    DATUM:    23.05.2003                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * AVB00666!23.05.03!2.1 ! NEUERSTELLUNG             ! MUND     *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0659                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0659.
          05 PI-DRVB0659-MA-ST-NR           PIC S9(009) COMP.
          05 PI-DRVB0659-MS-NR              PIC S9(004) COMP.
          05 PI-DRVB0659-DATE-DAT-VON       PIC  X(010).
      *
       01 PO-DRVB0659.
          05 PO-DRVB0659-RC                 PIC X(003).
             88 PO-DRVB0659-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0659-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0659-DATEN.
             10 PO-DRVB0659-BS-NR           PIC S9(004) COMP.
      *
      ****************** ENDE COPY-MEMBER DRVB0659 *********************
