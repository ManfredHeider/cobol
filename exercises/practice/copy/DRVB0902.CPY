      *---------------------------------------------------------------*
      *    Standard fuer alle Objekte unter PVCS VM
      *            Datum      :  02.05.2008
      *            Kategorie  :  Copystrecke
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2008 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0902.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0902.CPv  $
      *
      *   Rev 2.1   May 27 2008 13:44:04   t13ae27
      *AAS00756: Wettbewerb 2008 der Allfinanz / Team- und Zusatzbo
      *nus
      *
      *   Rev 2.0   May 02 2008 13:26:14   t13ae27
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0902                                      *
      *    --------------------------------------------------------   *
      *    SCHNITTSTELLE FÜR DAS UPRO ZXVB0082                        *
      *    MIT DEN DATEN, DIE EINMAL PRO UPRO ÜBERGEBEN WERDEN        *
      *    --------------------------------------------------------   *
      *    ERSTELLT: P.Duerr                                          *
      *    DATUM:    14.10.2003                                       *
      *****************************************************************
      * AENDERUNGEN:                                                 *
      * AUFTRAG ! DATUM  !VER ! AENDERUNG                 ! NAME     *
      * --------+--------+----+---------------------------+----------*
      * ASL00172!14.10.03! 2.1! neu                       ! P.Duerr  *
      *         !        !    !                           !          *
      * --------+--------+----+---------------------------+----------*
      *                                                              *
      * ENDE-LOGBUCH                                                 *
      ****************************************************************
      *    DIESES COPY ENTHÄLT DIE EIN- UND AUSGABEPARAMETER         *
      *    FÜR DIE FUNKTION DRVB0902                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0902.
          05 PI-DRVB0902-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0902-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0902-ANSPR-BIS          PIC  X(010).
          05 PI-DRVB0902-ANSPR-STATUS       PIC  X(001).
          05 PI-DRVB0902-KNR-TYP            PIC  9(003).
          05 PI-DRVB0902-WERTUNGS-TYP       PIC  9(003).
      *
       01 PO-DRVB0902.

          05 PO-DRVB0902-RC                 PIC  X(003).
             88 PO-DRVB0902-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0902-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0902-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0902-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0902-DATEN.
             10 PO-DRVB0902-LIST-MA-ST-NR      PIC S9(009) BINARY.
             10 PO-DRVB0902-LIST-MONAT         PIC  X(010).
             10 PO-DRVB0902-LIST-GEWINNER-KZ   PIC  X(001).
             10 PO-DRVB0902-LIST-SUM-BETRAG    PIC S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0902 *****************************
