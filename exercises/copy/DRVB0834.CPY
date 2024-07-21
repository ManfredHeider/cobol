      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  06.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0834.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0834.CPv  $
      *
      *   Rev 2.2   19 Dec 2006 12:49:24   t13ae13
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 09:16:40   t13ex43
      *AVB00868: VB45 - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Sep 06 2006 17:34:00   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0834                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0006                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    06.09.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  ! AENDERUNG                      ! NAME      *
      * --------+--------+--------------------------------+-----------*
      * AVB00868!06.09.06! NEU                            ! KLEEMEIER *
101121* VLA-2735!10.11.21! DIR Copy auf VIVBB006 erweitert! Bruder    *
      *         !        !                                !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0834 - ERMITTELN DIR-SATZ FÜR DIREKTION       *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0834.
           05  PI-DRVB0834-DATEN.
               10  PI-DRVB0834-DIR-NR            PIC S9(004) COMP.
      *
       01  PO-DRVB0834.
           05  PO-DRVB0834-RC                    PIC  X(003).
               88  PO-DRVB0834-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0834-RC-NOT-FOUND           VALUE 'NFD'.
101121     05  PO-DRVB0834-NI-VIVBB006.
               10  NI-DRVB0834-DIR-NR            PIC S9(004) COMP.
               10  NI-DRVB0834-DIR-LAST-UPDATE   PIC S9(004) COMP.
               10  NI-DRVB0834-KTY-DIR-TYP       PIC S9(004) COMP.
               10  NI-DRVB0834-KNR-DIR-TYP       PIC S9(004) COMP.
               10  NI-DRVB0834-KTY-REGION        PIC S9(004) COMP.
               10  NI-DRVB0834-KNR-REGION        PIC S9(004) COMP.
               10  NI-DRVB0834-KTY-RESERV-TYP    PIC S9(004) COMP.
               10  NI-DRVB0834-KNR-RESERV-TYP    PIC S9(004) COMP.
               10  NI-DRVB0834-MA-ST-NR          PIC S9(004) COMP.
               10  NI-DRVB0834-DIR-NAME          PIC S9(004) COMP.
               10  NI-DRVB0834-DIR-INFORMATION   PIC S9(004) COMP.
               10  NI-DRVB0834-KTY-VERTRBER      PIC S9(004) COMP.
               10  NI-DRVB0834-KNR-VERTRBER      PIC S9(004) COMP.
101121         10  NI-DRVB0834-KTY-REGDB         PIC S9(004) COMP.     
101121         10  NI-DRVB0834-KNR-REGDB         PIC S9(004) COMP.     
101121     05  PO-DRVB0834-DCL-VIVBB006.
               10  PO-DRVB0834-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB0834-DIR-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB0834-KTY-DIR-TYP       PIC S9(004) COMP.
               10  PO-DRVB0834-KNR-DIR-TYP       PIC S9(004) COMP.
               10  PO-DRVB0834-KTY-REGION        PIC S9(004) COMP.
               10  PO-DRVB0834-KNR-REGION        PIC S9(004) COMP.
               10  PO-DRVB0834-KTY-RESERV-TYP    PIC S9(004) COMP.
               10  PO-DRVB0834-KNR-RESERV-TYP    PIC S9(004) COMP.
               10  PO-DRVB0834-MA-ST-NR          PIC S9(009) COMP.
               10  PO-DRVB0834-DIR-NAME          PIC  X(040).
               10  PO-DRVB0834-DIR-INFORMATION   PIC  X(150).
               10  PO-DRVB0834-KTY-VERTRBER      PIC S9(004) COMP.
               10  PO-DRVB0834-KNR-VERTRBER      PIC S9(004) COMP.
101121         10  PO-DRVB0834-KTY-REGDB         PIC S9(004) COMP.
101121         10  PO-DRVB0834-KNR-REGDB         PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0834 ****************************************
      ****
