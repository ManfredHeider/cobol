      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  15.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0835.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0835.CPv  $
      *
      *   Rev 2.2   19 Dec 2006 12:54:44   t13ae13
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 09:17:06   t13ex43
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Sep 15 2006 17:14:54   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0835                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0055                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    18.09.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00868!18.09.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0835 - ERMITTELN AKTUELLEN MAST-SATZ          *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0835.
           05  PI-DRVB0835-DATEN.
               10  PI-DRVB0835-MA-ST-NR          PIC S9(009) COMP.
               10  PI-DRVB0835-KTY-MAST-TYP      PIC S9(004) COMP.
               10  PI-DRVB0835-KNR-MAST-TYP      PIC S9(004) COMP.
      *
       01  PO-DRVB0835.
           05  PO-DRVB0835-RC                    PIC  X(003).
               88  PO-DRVB0835-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0835-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0835-NI-VIVB0055.
               10  NI-DRVB0835-MA-ST-NR          PIC S9(004) COMP.
               10  NI-DRVB0835-MAST-DAT-VON      PIC S9(004) COMP.
               10  NI-DRVB0835-KTY-MAST-TYP      PIC S9(004) COMP.
               10  NI-DRVB0835-KNR-MAST-TYP      PIC S9(004) COMP.
               10  NI-DRVB0835-MAST-LAST-UPDATE  PIC S9(004) COMP.
               10  NI-DRVB0835-MAST-DAT-BIS      PIC S9(004) COMP.
           05  PO-DRVB0835-DCL-VIVB0055.
               10  PO-DRVB0835-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0835-MAST-DAT-VON      PIC  X(026).
               10  PO-DRVB0835-KTY-MAST-TYP      PIC S9(004) COMP.
               10  PO-DRVB0835-KNR-MAST-TYP      PIC S9(004) COMP.
               10  PO-DRVB0835-MAST-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0835-MAST-DAT-BIS      PIC  X(010).
      ****
      **** ENDE COPY DRVB0835 ****************************************
      ****
