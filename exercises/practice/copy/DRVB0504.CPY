      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0504.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0504.CPv  $
      *
      *   Rev 2.2   Jun 05 2003 10:41:56   t13ex22
      *AVB00666: AL-Zusätze anzeigen
      *
      *   Rev 2.1   06 Oct 2000 10:40:34   T13AE30
      * 
      *
      *   Rev 2.0   07 Sep 2000 11:32:04   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0504                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FUER DAS UPRO ZWVB0012                       *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO UEBERGEBEN WERDEN       *
      *    --------------------------------------------------------   *
      *    ERSTELLT: TH.WILLIG                                        *
      *    DATUM:    20.09.2000                                       *
      *****************************************************************
      *    VERSION  1
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00067!20.09.00!2.1 ! NEUERSTELLUNG             ! TH.WILLIG*
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FUER DIE FUNKTION DRVB0504                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0504.
          05 PI-DRVB0504-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0504.
          05 PO-DRVB0504-RC                 PIC X(003).
             88 PO-DRVB0504-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0504-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0504-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0504-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0504-DATEN.
             10 PO-DRVB0504-MS-NR           PIC S9(004) COMP.
             10 PO-DRVB0504-BS-NR           PIC S9(004) COMP.
             10 PO-DRVB0504-DATE-DAT-VON    PIC  X(010).
      *
      ****************** ENDE COPY-MEMBER DRVB0504 *********************
