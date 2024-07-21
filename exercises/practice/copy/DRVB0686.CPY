      *---------------------------------------------------------------*
      *    Standard für alle Objekte unter PVCS
      *            Datum   :  Januar 2003
      *            Kopf aus:  N:\PROFILE\KOPFCPY.DAT
      *---------------------------------------------------------------*
      *
      *---------------------------------------------------------------*
      *    Copyright (c) 2003 ATLAS Dienstleistungen f. V. GmbH
      *---------------------------------------------------------------*
      *    $Workfile:   DRVB0686.CPY  $
      *    $Revision:   2.1  $
      *
      *    $Log:   L:/mf/CPY/DRVB0686.CPv  $
      *
      *   Rev 2.1   Dec 10 2003 09:58:40   t13ae27
      *OLB-Auszahlung
      *
      *   Rev 2.0   Feb 17 2003 11:00:40   p13sp16
      *Archiverstellung
      *
      *---------------------------------------------------------------*
      *---------------------------------------------------------------*
      *    COPY-MEMBER: DRVB0686                                      *
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
      *    FÜR DIE FUNKTION DRVB0686                                 *
      *    AUSSER: DEM FUNKTIONSNAMEN,                               *
      *            DEM ERR-ERROR-BEREICH UND                         *
      *            DEM STANDARD-STATUS-BEREICH                       *
      *            (JEWEILS SEPARATE COPYS)                          *
      *                                                              *
      ****************************************************************
      *
       01 PI-DRVB0686.
          05 PI-DRVB0686-UFKT-CODE          PIC  9(002).
          05 PI-DRVB0686-ANSPR-VOM          PIC  X(010).
          05 PI-DRVB0686-ANSPR-STATUS       PIC  X(001).
          05 PI-DRVB0686-KNR-TYP            PIC  9(003).
          05 PI-DRVB0686-WERTUNGS-TYP       PIC  9(003).
      *
       01 PO-DRVB0686.

          05 PO-DRVB0686-RC                 PIC  X(003).
             88 PO-DRVB0686-RC-OK                        VALUE 'OK '.
             88 PO-DRVB0686-RC-LEER                      VALUE 'LER'.
             88 PO-DRVB0686-RC-EOF                       VALUE 'EOF'.
             88 PO-DRVB0686-RC-NOT-FOUND                 VALUE 'NFD'.

          05 PO-DRVB0686-DATEN.
             10 PO-DRVB0686-LIST-MA-ST-NR      PIC S9(009) BINARY.
             10 PO-DRVB0686-LIST-MONAT         PIC  X(010).
             10 PO-DRVB0686-LIST-GEWINNER-KZ   PIC  X(001).
             10 PO-DRVB0686-LIST-SUM-BETRAG    PIC S9(09)V9(02) COMP-3.
      *
      ***** ENDE COPY-MEMBER DRVB0686 *****************************
