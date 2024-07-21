      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 1999
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright 1999 ATLAS Dienstleistungs GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0506.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:\MF\CPY\DRVB0506.CPv  $
      *
      *   Rev 2.1   06 Oct 2000 10:40:38   T13AE30
      * 
      *
      *   Rev 2.0   07 Sep 2000 11:32:06   P13SP13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0506                                      *
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
      *    FUER DIE FUNKTION DRVB0506                                *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
      *
       01 PI-DRVB0506.
          05 PI-DRVB0506-ZEITPUNKT          PIC  X(010).
          05 PI-DRVB0506-MS-NR              PIC S9(004) COMP.
          05 PI-DRVB0506-MA-ST-NR           PIC S9(009) COMP.
      *
       01 PO-DRVB0506.
          05 PO-DRVB0506-RC                 PIC X(003).
             88 PO-DRVB0506-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0506-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0506-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0506-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0506-DATEN.
             10 PO-DRVB0506-DATE-DAT-VON    PIC  X(010).
      *
      ****************** ENDE COPY-MEMBER DRVB0506 *********************
