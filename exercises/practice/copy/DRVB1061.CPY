      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  31.07.2012
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2012 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB1061.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB1061.CPv  $
      *
      *   Rev 2.1   Nov 22 2012 09:07:30   t13ae23
      *ASL00460: SL-Dialog: Erf.Sonderfälle CoBo
      *
      *   Rev 2.0   Jul 31 2012 13:54:32   t13ae23
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *****************************************************************
      *    COPY-MEMBER: DRVB1061                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DIE FUNKTION DRVB1061                    *
      *    --------------------------------------------------------   *
      *    ERSTELLT: V. RAHNER                                        *
      *    DATUM:    08.2012                                          *
      *****************************************************************
      *    VERSION
      ****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * -------------------------------------------------------------*
      * ASL00460!14.11.12!2.1 ! NEU                       ! RAHNER   *
      *         !        !    !                           !          *
      *         !        !    !                           !          *
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER
      *    FÜR DIE FUNKTION DRVB1061: LESEN EINTRÄGE ZU COLUMBUSBONUS
      *    AUSSER: DEM FUNKTIONSNAMEN,
      *            DEM ERR-ERROR-BEREICH UND
      *            DEM STANDARD-STATUS-BEREICH
      *            (JEWEILS SEPARATE COPYS)
      *
      ****************************************************************
       01  PI-DRVB1061.
           05 PI-DRVB1061-PROGRAMM          PIC  X(008).
           05 PI-DRVB1061-UFKT-CODE         PIC  9(002).
           05 PI-DRVB1061-MA-ST-NR          PIC  S9(007)  COMP-3.
      *
       01  PO-DRVB1061.

           05 PO-DRVB1061-RC                PIC  X(003).
             88 PO-DRVB1061-RC-OK                        VALUE 'OK '.
             88 PO-DRVB1061-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB1061-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB1061-RC-ERR                       VALUE 'ERR'.

           05 PO-DRVB1061-DATEN.
             10 PO-DRVB1061-KAND-NEU-AB      PIC  X(010).
             10 PO-DRVB1061-KAND-TAETIG-BIS  PIC  X(010).
             10 PO-DRVB1061-KAND-WERTUNGSTUF PIC  S9(004) COMP.
             10 PO-DRVB1061-KAND-MINDEST-EH  PIC  S9(003) COMP.
             10 PO-DRVB1061-KAND-TYP         PIC  X(001).
             10 PO-DRVB1061-KAND-DATUM-RUECK PIC  X(010).
             10 PO-DRVB1061-KAND-GRUND       PIC  X(001).
             10 PO-DRVB1061-KAND-KZ-R        PIC  X(001).
             10 PO-DRVB1061-KAND-BLOCK-NR    PIC  9(001).
             10 PO-DRVB1061-KAND-LAST-UPDATE PIC  X(026).
