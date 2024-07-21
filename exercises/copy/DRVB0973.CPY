      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  12.03.2010
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2010 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0973.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0973.CPv  $
      *
      *   Rev 2.1   Apr 06 2010 13:31:46   t13ex13
      *ASL00344: RGT: SL08
      *
      *   Rev 2.0   Mar 12 2010 09:34:50   t13ex13
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0973                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0200                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: WALB                                             *
      *    DATUM:    12.03.2010                                       *
      *****************************************************************
      * AENDERUNGEN:                                                  *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME      *
      * --------+--------+----+---------------------------+-----------*
      * ASL00344!12.03.10!2.1 ! NEU                       !GUTFLEISCH *
      *         !        !    !                           !           *
      *                                                               *
      *****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER          *
      *    FÜR DIE FUNKTION DRVB0973                                  *
      *    AUSSER: DEM FUNKTIONSNAMEN,                                *
      *            DEM ERR-ERROR-BEREICH UND                          *
      *            DEM STANDARD-STATUS-BEREICH                        *
      *            (JEWEILS SEPARATE COPYS)                           *
      *                                                               *
      *****************************************************************
      *
       01 PI-DRVB0973.
          05 PI-DRVB0973-UFKT-CODE             PIC  9(002).
          05 PI-DRVB0973-MA-ST-NR              PIC S9(009) BINARY.
          05 PI-DRVB0973-CBA-PART-VBNR         PIC S9(009) BINARY.
          05 PI-DRVB0973-CBA-ANSPR-VON         PIC  X(010).
          05 PI-DRVB0973-CBA-ANSPR-BIS         PIC  X(010).
          05 PI-DRVB0973-KNR-ANSPR-STATUS      PIC S9(004) BINARY.
      *
       01 PO-DRVB0973.
          05 PO-DRVB0973-RC                    PIC  X(003).
             88 PO-DRVB0973-RC-OK                   VALUE 'OK '.
             88 PO-DRVB0973-RC-EOF                  VALUE 'EOF'.
          05 PO-DRVB0973-DATEN.
             10 PO-DRVB0973-MA-ST-NR           PIC S9(009) BINARY.
             10 PO-DRVB0973-CBA-ANSPR-VOM      PIC  X(010).
             10 PO-DRVB0973-CBA-PART-VBNR      PIC S9(009) BINARY.
             10 PO-DRVB0973-KNR-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0973-KTY-ANSPR-GRUND    PIC S9(004) BINARY.
             10 PO-DRVB0973-CBA-LAST-UPDATE    PIC  X(026).
             10 PO-DRVB0973-KTY-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0973-KNR-WAEHRUNG       PIC S9(004) BINARY.
             10 PO-DRVB0973-CBA-PART-BETRAG    PIC S9(09)V9(02) COMP-3.
             10 PO-DRVB0973-CBA-PART-EH        PIC S9(09)V9 COMP-3.
             10 PO-DRVB0973-CBA-PART-SEIT      PIC  X(010).
             10 PO-DRVB0973-KTY-ANSPR-STATUS   PIC S9(004) BINARY.
             10 PO-DRVB0973-KNR-ANSPR-STATUS   PIC S9(004) BINARY.
      *
      ***** ENDE COPY-MEMBER DRVB0973 *****************************
      *
