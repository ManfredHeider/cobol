      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  03.08.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0986.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0986.CPv  $
      *
      *   Rev 2.1   Aug 26 2010 11:29:48   t13ex13
      *ASL00354: Columbus-Bonus für FVD
      *
      *   Rev 2.0   Aug 03 2010 15:18:58   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0986                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0201                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: GUTFLEISCH                                       *
      *    DATUM:    05.08.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00354!05.08.10!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0986                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0986.
          05 PI-DRVB0986-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0986-MA-ST-NR              PIC S9(009) BINARY.
          05 PI-DRVB0986-CBAN-PART-VBNR        PIC S9(009) BINARY.
          05 PI-DRVB0986-CBAN-ANSPR-VON        PIC  X(010).
          05 PI-DRVB0986-CBAN-ANSPR-BIS        PIC  X(010).
          05 PI-DRVB0986-KNR-NERF-STATUS       PIC S9(004) BINARY.
      *
       01 PO-DRVB0986.
          05 PO-DRVB0986-RC                    PIC  X(003).
             88 PO-DRVB0986-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0986-RC-EOF                  VALUE 'EOF'.
          05 PO-DRVB0986-DATEN.
             10 PO-DRVB0986-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0986-CBAN-ANSPR-VOM     PIC  X(010).
             10 PO-DRVB0986-CBAN-PART-VBNR     PIC S9(009) BINARY.
             10 PO-DRVB0986-KNR-NERF-GRUND     PIC S9(004) BINARY.
             10 PO-DRVB0986-KTY-NERF-GRUND     PIC S9(004) BINARY.
             10 PO-DRVB0986-CBAN-LAST-UPDATE   PIC  X(026).
             10 PO-DRVB0986-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0986-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0986-CBAN-PART-BETRAG   PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0986-CBAN-PART-EH       PIC S9(09)V9 COMP-3.
             10 PO-DRVB0986-CBAN-PART-SEIT     PIC  X(010).
             10 PO-DRVB0986-KTY-NERF-STATUS    PIC S9(004) BINARY.
             10 PO-DRVB0986-KNR-NERF-STATUS    PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0986 *****************************
      *
