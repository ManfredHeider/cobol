      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.07.2004
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2004 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0719.CPY  $
      *    $Revision:   2.2  $
      *
      *    $Log:   L:/mf/CPY/DRVB0719.CPv  $
      *
      *   Rev 2.2   19 Dec 2006 09:58:22   t13ae13
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.1   Nov 09 2006 08:58:48   t13ex43
      *AVB00868: PGM - Host-Pflegedialog DG und V.-Ber.
      *
      *   Rev 2.0   Jul 02 2004 15:58:22   t13ex43
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB0719                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS JOIN-OZP ZJVB0026                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: KLEEMEIER                                        *
      *    DATUM:    28.07.2006                                       *
      *****************************************************************
      *    VERSION
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * AVB00868!28.07.06!2.1 ! NEU                       ! KLEEMEIER *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *                                                               *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER FÜR DIE  *
      *    FUNKTION DRVB0719 - ERMITTELN MITGLIEDER EINER DIR-GEMEIN. *
      *                                                               *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01  PI-DRVB0719.
           05  PI-DRVB0719-DATEN.
               10  PI-DRVB0719-UFKT-CODE         PIC  9(002).
               10  PI-DRVB0719-DG-BEZ            PIC  X(010).
      *
       01  PO-DRVB0719.
           05  PO-DRVB0719-RC                    PIC  X(003).
               88  PO-DRVB0719-RC-OK                  VALUE 'OK '.
               88  PO-DRVB0719-RC-EOF                 VALUE 'EOF'.
           05  PO-DRVB0719-DATEN.
               10  PO-DRVB0719-VBER-BEZ          PIC  X(010).
               10  PO-DRVB0719-DIR-NR            PIC S9(004) COMP.
               10  PO-DRVB0719-DL-VBNR           PIC S9(009) COMP.
               10  PO-DRVB0719-DL-NAME           PIC  X(030).
               10  PO-DRVB0719-DIR-LAST-UPDATE   PIC  X(026).
               10  PO-DRVB0719-MAST-LAST-UPDATE  PIC  X(026).
               10  PO-DRVB0719-DG-SPRECHER       PIC  X(001).
      ****
      **** ENDE COPY DRVB0719 ****************************************
      ****
