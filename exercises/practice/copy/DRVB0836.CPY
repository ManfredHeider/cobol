      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  27.09.2006
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2006 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0836.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0836.CPv  $
      *
      *   Rev 2.2   19 Dec 2006 10:03:46   t13ae13
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 08:59:12   t13ex43
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Sep 27 2006 17:25:36   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0836                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS LESE-OZP ZWVB0006                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    27.09.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00868!27.09.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0836 - ERMITTELN VERTRIEBSBEREICH FÜR DG      *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0836.
           05  PI-DRVB0836-DATEN.
               10  PI-DRVB0836-KTY-REGION        PIC S9(004) COMP.
               10  PI-DRVB0836-KNR-REGION        PIC S9(004) COMP.
      *
       01  PO-DRVB0836.
           05  PO-DRVB0836-RC                    PIC  X(003).
               88  PO-DRVB0836-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0836-RC-NOT-FOUND           VALUE 'NFD'.
           05  PO-DRVB0836-DATEN.
               10  PO-DRVB0836-KTY-VERTRBER      PIC S9(004) COMP.
               10  PO-DRVB0836-KNR-VERTRBER      PIC S9(004) COMP.
      ****
      **** ENDE COPY DRVB0836 ****************************************
      ****
